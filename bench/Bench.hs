{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}
module Main where

import Control.Concurrent (threadDelay, yield)
import Control.Concurrent.Async (async, cancel, wait)
import Control.Concurrent.STM
import Control.Exception (bracket, evaluate)
import Control.Monad
import Criterion
import Criterion.Main
import Data.List (sort)
import Data.Queue
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import qualified Control.Concurrent.STM.TBQueue as B
import qualified Control.Concurrent.STM.TQueue as T
import System.Environment (getArgs)
import System.Mem

makeBackedUpQueue :: Int -> IO (Queue Int)
makeBackedUpQueue n = atomically do
  q <- newQueue
  forM_ [1..n] (enqueue q)
  pure q

makeBackedUpTQueue :: Int -> IO (T.TQueue Int)
makeBackedUpTQueue n = atomically do
  q' <- T.newTQueue
  forM_ [1..n] (T.writeTQueue q')
  pure q'

makeBackedUpBoundedQueue :: Int -> IO (Queue Int)
makeBackedUpBoundedQueue n = atomically do
  q <- newBoundedQueue (fromIntegral n)
  forM_ [1..n] (enqueue q)
  pure q

makeBackedUpTBQueue :: Int -> IO (B.TBQueue Int)
makeBackedUpTBQueue n = atomically do
  q <- B.newTBQueue (fromIntegral n)
  forM_ [1..n] (B.writeTBQueue q)
  pure q

burstBenchmarks :: Int -> [Benchmark]
burstBenchmarks n =
  [ bench ("produce " <> show n <> " then consume from Queue") $ nfIO do
      q <- makeBackedUpQueue n
      atomically (dequeue q)
  , bench ("produce " <> show n <> " then consume from TQueue") $ nfIO do
      q <- makeBackedUpTQueue n
      atomically (T.readTQueue q)
  ]

boundedBurstBenchmarks :: Int -> [Benchmark]
boundedBurstBenchmarks n =
  [ bench ("produce " <> show n <> " then consume from bounded Queue") $ nfIO do
      q <- makeBackedUpBoundedQueue n
      atomically (dequeue q)
  , bench ("produce " <> show n <> " then consume from TBQueue") $ nfIO do
      q <- makeBackedUpTBQueue n
      atomically (B.readTBQueue q)
  ]

creationBenchmarks :: [Benchmark]
creationBenchmarks =
  [ bench "newQueue via atomically" $ whnfIO (atomically (newQueue @Int))
  , bench "newQueueIO" $ whnfIO (newQueueIO @Int)
  , bench "newBoundedQueue via atomically" $ whnfIO (atomically (newBoundedQueue @Int 1024))
  , bench "newBoundedQueueIO" $ whnfIO (newBoundedQueueIO @Int 1024)
  ]

latencySampleCount :: Int
latencySampleCount = 20

latencyBatchSize :: Int -> Int
latencyBatchSize queueLength = max 1 (100000 `div` queueLength)

measurePostBurstLatency :: Int -> IO q -> (q -> STM Int) -> IO Word64
measurePostBurstLatency batchSize makeQueue readQueue = do
  queues <- replicateM batchSize makeQueue
  started <- getMonotonicTimeNSec
  forM_ queues \q -> do
    value <- atomically (readQueue q)
    _ <- evaluate value
    pure ()
  finished <- getMonotonicTimeNSec
  pure ((finished - started) `div` fromIntegral batchSize)

percentile :: Double -> [Word64] -> Word64
percentile fraction samples =
  sort samples !! (ceiling (fraction * fromIntegral (length samples)) - 1)

printLatencySummary :: String -> [Word64] -> IO ()
printLatencySummary label samples =
  putStrLn
    (  label
    <> " per-operation batch estimate: median " <> show (percentile 0.50 samples) <> " ns"
    <> ", p95 " <> show (percentile 0.95 samples) <> " ns"
    <> ", p99 " <> show (percentile 0.99 samples) <> " ns"
    )

postBurstLatencyReport :: Int -> IO ()
postBurstLatencyReport n = do
  putStrLn ("First-consume latency after producing " <> show n <> " items:")
  performGC
  let batchSize = latencyBatchSize n
  samples <- forM [1..latencySampleCount] \sampleNumber ->
    if even sampleNumber
      then do
        queueSample <- measurePostBurstLatency batchSize (makeBackedUpQueue n) dequeue
        tQueueSample <- measurePostBurstLatency batchSize (makeBackedUpTQueue n) T.readTQueue
        pure (queueSample, tQueueSample)
      else do
        tQueueSample <- measurePostBurstLatency batchSize (makeBackedUpTQueue n) T.readTQueue
        queueSample <- measurePostBurstLatency batchSize (makeBackedUpQueue n) dequeue
        pure (queueSample, tQueueSample)
  let (queueSamples, tQueueSamples) = unzip samples
  printLatencySummary "Queue" queueSamples
  printLatencySummary "TQueue" tQueueSamples

benchmarkDurationMicros :: Int
benchmarkDurationMicros = 1500000

howManyCooks
  :: IO q
  -> (q -> STM a)
  -> (q -> Int -> STM b)
  -> Int
  -> IO (Int, Int)
howManyCooks newQ readQ writeQ threadCount = do
  q <- newQ
  ready <- newTVarIO 0
  start <- newEmptyTMVarIO
  stop <- newTVarIO False
  let
    consumer count = do
      next <- atomically do
        stopped <- readTVar stop
        if stopped
          then pure Nothing
          else Just <$> readQ q
      case next of
        Nothing -> pure count
        Just _ -> do
          yield
          consumer (count + 1)

    producer count = do
      continue <- atomically do
        stopped <- readTVar stop
        if stopped
          then pure False
          else writeQ q count >> pure True
      if continue
        then do
          yield
          producer (count + 1)
        else pure count

    worker i = do
      atomically (modifyTVar' ready (+ 1))
      atomically (readTMVar start)
      if even i
        then do
          readCount <- consumer 0
          pure (0, readCount)
        else do
          writeCount <- producer 0
          pure (writeCount, 0)

  bracket
    (forM [1..threadCount] (async . worker))
    (mapM_ cancel)
    \workers -> do
      atomically do
        readyCount <- readTVar ready
        check (readyCount == threadCount)
        putTMVar start ()
      threadDelay benchmarkDurationMicros
      atomically (writeTVar stop True)
      counts <- mapM wait workers
      let writeCount = sum (map fst counts)
          readCount = sum (map snd counts)
      putStrLn ("Observed " <> show writeCount <> " writes")
      putStrLn ("Observed " <> show readCount <> " reads")
      pure (writeCount, readCount)

relativeDifference :: Int -> Int -> Double
relativeDifference measured baseline =
  fromIntegral (measured - baseline) / fromIntegral baseline


throughputTest :: Int -> IO ()
throughputTest n = do
  putStrLn ("Running a throughput test for " <> show n <> " threads...")
  putStrLn "Queue: "
  performGC
  (queueWrites, queueReads) <- howManyCooks (makeBackedUpQueue 0) dequeue enqueue n
  putStrLn "TQueue: "
  performGC
  (tQueueWrites, tQueueReads) <- howManyCooks (makeBackedUpTQueue 0) T.readTQueue T.writeTQueue n
  putStrLn ("Queue reads - TQueue reads over TQueue reads: " <> show (relativeDifference queueReads tQueueReads))
  putStrLn ("Queue writes - TQueue writes over TQueue writes: " <> show (relativeDifference queueWrites tQueueWrites))

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--throughput"] ->
      sequence_ [ throughputTest n | n <- [2^i | i <- [1..12 :: Int]] ]
    ["--latency"] ->
      mapM_ postBurstLatencyReport [100, 1000, 10000]
    _ ->
      defaultMain
        (  creationBenchmarks
        <> burstBenchmarks 100
        <> burstBenchmarks 1000
        <> burstBenchmarks 10000
        <> boundedBurstBenchmarks 100
        <> boundedBurstBenchmarks 1000
        <> boundedBurstBenchmarks 10000
        )
