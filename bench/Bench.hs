{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE BlockArguments #-}
module Main where

import Control.Concurrent
import Data.Time.Clock
import Data.Queue
import Control.Monad
import Criterion
import Criterion.Main
import Control.Concurrent.STM
import qualified Control.Concurrent.STM.TQueue as T
import Control.Exception (evaluate)
import Control.DeepSeq
import System.Timeout
import Data.IORef

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

howManyCooks newQ readQ writeQ n = do
  q <- atomically newQ
  let
    consumer t n = do
      t' <- getCurrentTime
      if t' < t then do
        void $ atomically (readQ q)
        yield
        consumer t (n + 1)
      else do
        pure n
    producer t n = do
      t' <- getCurrentTime
      if t' < t then do
        void $ atomically (writeQ q n)
        yield
        producer t (n + 1)
      else do
        pure n

  writeRef <- newIORef 0
  readRef <- newIORef 0
  forM_ [1..n] \i -> forkIO $ void $ do
    t <- (10 `addUTCTime`) <$> getCurrentTime
    if i `mod` 2 == 0 then do
      n <- consumer t 0
      atomicModifyIORef readRef (\m -> (n + m, ()))
    else do
      n <- producer t 0
      atomicModifyIORef writeRef (\m -> (n + m, ()))
  threadDelay (12 * 1000000)
  writes <- readIORef writeRef
  reads <- readIORef readRef
  putStrLn ("Observed " <> show writes <> " writes")
  putStrLn ("Observed " <> show reads <> " reads")
  pure (writes, reads)


throughputTest :: Int -> IO ()
throughputTest n = do
  putStrLn ("Running a throughput test for " <> show n <> " threads...")
  putStrLn "Queue: "
  (writes, reads) <- howManyCooks (newQueue @Int) dequeue enqueue n
  putStrLn "TQueue: "
  (writes', reads') <- howManyCooks (T.newTQueue @Int) T.readTQueue T.writeTQueue n
  putStrLn ("Queue reads - TQueue reads: " <> show (reads - reads'))
  putStrLn ("Queue writes - TQueue writes: " <> show (writes - writes'))
  

main :: IO ()
main = do
  throughputTest 2
  throughputTest 4
  throughputTest 8
  throughputTest 16
  throughputTest 32
  throughputTest 64
  throughputTest 128
  throughputTest 256
  defaultMain
    (  backedUpBenchmarks 100
    <> backedUpBenchmarks 1000
    <> backedUpBenchmarks 10000
    )
