{-# LANGUAGE DataKinds, LambdaCase, ViewPatterns, GADTs, Rank2Types, ScopedTypeVariables, TypeApplications #-}
module Main where

import Control.Concurrent.STM
import Control.Market
import Text.Read
import Data.Proxy
import Control.Monad
import qualified Data.Heap as H
import System.IO

execution :: Direction dir -> String
execution BidV = "sold"
execution OfferV = "bought"

main :: IO ()
main = do
  vMarket <- newTVarIO emptyMarket
  vLog <- newTVarIO []
  let contract :: forall dir. KnownDirection dir => Order dir String -> String -> STM ()
      contract o b = modifyTVar vLog $ (:) $ unwords
        [b, execution (directionVal $ Proxy @ dir), showOrder id o]
  forever $ do
    Market bids offers <- atomically $ readTVar vMarket
    let best :: KnownDirection dir => H.Heap (Order dir String) -> String
        best = maybe "" (showOrder id . fst) . H.viewMin
    putStr $ best bids ++ " / " ++ best offers
    putStr "> "
    hFlush stdout
    let append :: KnownDirection dir => Maybe (Order dir String) -> STM ()
        append = modifyTVar vLog . maybe id ((:) . showOrder id)
    words <$> getLine >>= \case
      [user, ty, readMaybe -> Just qty, '@' : (readMaybe -> Just price)] -> case ty of
        "bids" -> atomically $ place vMarket contract (Order qty price user :: Order 'Bid String) >>= append
        "offers" -> atomically $ place vMarket contract (Order qty price user :: Order 'Offer String) >>= append
        "buys" -> atomically $ trade vMarket contract (Order qty price user :: Order 'Offer String) >>= append
        "sells" -> atomically $ trade vMarket contract (Order qty price user :: Order 'Bid String) >>= append
        _ -> putStrLn "unknown command"
      _ -> putStrLn "unknown command"
    ls <- atomically $ do
      ls <- readTVar vLog
      writeTVar vLog []
      return $ reverse ls
    mapM_ putStrLn ls
