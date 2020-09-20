{-# LANGUAGE BlockArguments #-}
module Main where

import Data.Queue
import Control.Monad
import Criterion
import Criterion.Main
import Control.Concurrent.STM
import qualified Control.Concurrent.STM.TQueue as T
import Control.Exception (evaluate)
import Control.DeepSeq

data Stop x = Stop x

instance NFData (Stop x) where
  rnf stop@(Stop _) = ()

makeBackedUpQueue :: Int -> IO (Queue Int)
makeBackedUpQueue n = atomically do
  q <- newQueue
  forM_ [1..n] \i -> do
    enqueue q i
  pure q

makeBackedUpTQueue :: Int -> IO (T.TQueue Int)
makeBackedUpTQueue n = atomically do
  q' <- T.newTQueue
  forM_ [1..n] \i -> do
    T.writeTQueue q' i
  pure q'

backedUpBenchmarks n =
  [ bench ("dequeue from length " <> show n <> " backed up Queue") $ perRunEnv (Stop <$> makeBackedUpQueue n) \(Stop q) -> do
      evaluate =<< (atomically . dequeue) q
  , bench ("readTQueue from length " <> show n <> " backed up TQueue") $ perRunEnv (Stop <$> makeBackedUpTQueue n) \(Stop q') -> do
      evaluate =<< (atomically . readTQueue) q'
  ]

main :: IO ()
main = defaultMain
  (  backedUpBenchmarks 100
  <> backedUpBenchmarks 1000
  <> backedUpBenchmarks 10000
  )
