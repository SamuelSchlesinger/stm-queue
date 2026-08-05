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
