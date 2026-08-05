{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}
module Main where

import Test.Hspec
import Control.Concurrent.STM
import Control.Monad
import Data.Queue
import System.Timeout

main :: IO ()
main = hspec $ do
  describe "Data.Queue" do
    it "enqueues and dequeues a message" do
      msg <- atomically do
        q <- newQueue
        enqueue q "Hello"
        dequeue q
      msg `shouldBe` "Hello"
    it "creates a queue directly in IO" do
      q <- newQueueIO @String
      atomically (enqueue q "Hello")
      atomically (dequeue q) `shouldReturn` "Hello"
    it "creates a bounded queue directly in IO" do
      q <- newBoundedQueueIO 1
      atomically (tryEnqueue q "Hello") `shouldReturn` True
      atomically (tryEnqueue q "World") `shouldReturn` False
      atomically (dequeue q) `shouldReturn` "Hello"
    it "enqueues and tries to dequeue a message" do
      msg <- atomically do
        q <- newQueue
        enqueue q "Hello"
        tryDequeue q
      msg `shouldBe` Just "Hello"
    it "enqueues and tries to dequeue many messages" do
      msg <- atomically do
        q <- newQueue @Int
        forM_ [1..100] (enqueue q)
        forM_ [1..99 :: Int] (const . void $ tryDequeue q)
        tryDequeue q
      msg `shouldBe` Just 100
    it "Nothing can be dequeued from an empty queue" do
      msg <- atomically do
        q <- newQueue @String
        tryDequeue q
      msg `shouldBe` Nothing
    it "tries to peek at the top element" do
      msg <- atomically do
        q <- newQueue
        enqueue q "Hello"
        tryPeek q
      msg `shouldBe` Just "Hello"
    it "trying to peek an empty queue does the right thing" do
      msg <- atomically do
        q <- newQueue @Int
        tryPeek q
      msg `shouldBe` Nothing
    it "peeks at the top element" do
      msg <- atomically do
        q <- newQueue
        enqueue q "Hello"
        peek q
      msg `shouldBe` "Hello"
    it "enqueues and dequeues many messages" do
      msgs <- atomically do
        q <- newQueue @Int
        forM_ [1..10] (enqueue q)
        forM [1..10 :: Int] (const $ dequeue q)
      msgs `shouldBe` [1..10]
    it "dequeues the oldest item from a large backlog" do
      msg <- atomically do
        q <- newQueue @Int
        forM_ [1..100000] (enqueue q)
        dequeue q
      msg `shouldBe` 1
    it "all reads should block on an empty queue" do
      q <- atomically (newQueue @Int)
      let seconds n = n * 1000000
      timeout (seconds 1) (atomically (dequeue q))
        `shouldReturn` Nothing
      timeout (seconds 1) (atomically (peek q))
        `shouldReturn` Nothing
    it "flushes everything properly" do
      msgs <- atomically do
        q <- newQueue @Int
        forM_ [1..100] (enqueue q)
        flush q
      msgs `shouldBe` [1..100]
    it "applies transactional backpressure and preserves FIFO order" do
      q <- atomically (newBoundedQueue @Int 2)
      atomically (enqueue q 1)
      atomically (enqueue q 2)
      atomically (isFull q) `shouldReturn` True
      accepted <- atomically ((enqueue q 3 >> pure True) `orElse` pure False)
      accepted `shouldBe` False
      atomically (dequeue q) `shouldReturn` 1
      atomically (isFull q) `shouldReturn` False
      atomically (tryEnqueue q 3) `shouldReturn` True
      atomically (flush q) `shouldReturn` [2, 3]
    it "releases all bounded capacity when flushed" do
      q <- atomically (newBoundedQueue @Int 2)
      atomically (enqueue q 1 >> enqueue q 2)
      atomically (flush q) `shouldReturn` [1, 2]
      atomically (tryEnqueue q 3) `shouldReturn` True
      atomically (tryEnqueue q 4) `shouldReturn` True
      atomically (tryEnqueue q 5) `shouldReturn` False
    it "releases bounded capacity after tryDequeue" do
      q <- atomically (newBoundedQueue 1)
      atomically (enqueue q "first")
      atomically (tryDequeue q) `shouldReturn` Just "first"
      atomically (tryEnqueue q "second") `shouldReturn` True
      atomically (dequeue q) `shouldReturn` "second"
    it "rolls occupancy accounting back with its transaction" do
      q <- atomically (newBoundedQueue 1)
      atomically ((enqueue q "rolled back" >> retry) `orElse` pure ())
      atomically (tryEnqueue q "committed") `shouldReturn` True
      atomically ((dequeue q >> retry) `orElse` pure ())
      atomically (tryEnqueue q "too soon") `shouldReturn` False
      atomically (dequeue q) `shouldReturn` "committed"
    it "treats a zero-capacity queue as always full" do
      q <- atomically (newBoundedQueue 0)
      atomically (isFull q) `shouldReturn` True
      atomically (tryEnqueue q ()) `shouldReturn` False
      atomically ((enqueue q () >> pure True) `orElse` pure False)
        `shouldReturn` False
    it "allows tryEnqueue on an unbounded queue" do
      q <- atomically newQueue
      atomically (isFull q) `shouldReturn` False
      atomically (tryEnqueue q "Hello") `shouldReturn` True
      atomically (dequeue q) `shouldReturn` "Hello"
