module Control.Market where

import Control.Concurrent.STM
import Control.Monad
import qualified Data.Heap as H
import Text.Read
import Data.Proxy

data DirectionK = Bid | Offer

type family Counterpart (dir :: DirectionK) where
  Counterpart 'Bid = 'Offer
  Counterpart 'Offer = 'Bid

data Direction x where
  BidV :: Direction Bid
  OfferV :: Direction Offer

class KnownDirection (dir :: DirectionK) where
  directionVal :: proxy dir -> Direction dir

instance KnownDirection 'Bid where
  directionVal _ = BidV

instance KnownDirection 'Offer where
  directionVal _ = OfferV

type Qty = Int
type Price = Int

data Order (dir :: DirectionK) a = Order !Qty !Price a

instance Eq (Order dir a) where
  Order _ p _ == Order _ q _ = p == q

-- | less = more front
instance KnownDirection dir => Ord (Order dir a) where
  compare = case directionVal (Proxy @ dir) of
    BidV -> \(Order _ p _) (Order _ q _) -> compare q p
    OfferV -> \(Order _ p _) (Order _ q _) -> compare p q

counterpart :: Order dir a -> Order (Counterpart dir) a
counterpart (Order q p a) = Order q p a

fromCounterpart :: Order (Counterpart dir) a -> Order dir a
fromCounterpart (Order q p a) = Order q p a

data Market a = Market
  { _bids :: !(H.Heap (Order 'Bid a))
  , _offers :: !(H.Heap (Order 'Offer a))
  }

showOrder :: forall dir a. KnownDirection dir
  => (a -> String) -> Order dir a -> String
showOrder disp (Order q p a) = show q ++ "@" ++ show p
  ++ case directionVal (Proxy @ dir) of
    BidV -> " to " ++ disp a
    OfferV -> " from " ++ disp a

emptyMarket :: Market a
emptyMarket = Market H.empty H.empty

place :: forall dir a. (KnownDirection dir, KnownDirection (Counterpart dir))
  => TVar (Market a)
  -> (Order dir a -> a -> STM ())
  -> Order dir a -> STM (Maybe (Order dir a))
place vMarket contract order = case directionVal (Proxy :: Proxy dir) of
  BidV -> withTrade $ \order' ->
    modifyTVar' vMarket $ \mkt -> mkt { _bids = H.insert (counterpart order') $ _bids mkt }
  OfferV -> withTrade $ \order' ->
    modifyTVar' vMarket $ \mkt -> mkt { _offers = H.insert (counterpart order') $ _offers mkt }
  where
    withTrade :: (Order (Counterpart dir) a -> STM ()) -> STM (Maybe (Order dir a))
    withTrade k = trade vMarket (contract . fromCounterpart) (counterpart order)
      >>= \v -> fmap fromCounterpart v <$ mapM_ k v

trade :: forall dir a b. (KnownDirection dir)
  => TVar (Market a)
  -> (Order dir a -> b -> STM ())
  -> Order dir b
  -> STM (Maybe (Order dir b)) -- unexecuted
trade vMarket contract (Order total limit b) = do
  mkt <- readTVar vMarket
  case dir of
    BidV -> do
      (r, bids') <- go (<limit) total $ _bids mkt
      writeTVar vMarket $! mkt { _bids = bids' }
      return r
    OfferV -> do
      (r, offers') <- go (>limit) total $ _offers mkt
      writeTVar vMarket $! mkt { _offers = offers' }
      return r
  where
    dir = directionVal $ Proxy @ dir
    go :: (Price -> Bool) -> Qty -> H.Heap (Order dir a) -> STM (Maybe (Order dir b), H.Heap (Order dir a))
    go exit remaining h = case H.viewMin h of
      Nothing -> return (Just remainder, h)
      Just (full@(Order available p a), h')
        | exit p -> return (Just remainder, h)
        | otherwise -> case compare remaining available of
          LT -> (Nothing, H.insert (Order (available - remaining) p a) h')
            <$ contract (Order remaining p a) b
          EQ -> (Nothing, h') <$ contract full b
          GT -> contract full b >> go exit (remaining - available) h'
      where
        remainder = Order remaining limit b
