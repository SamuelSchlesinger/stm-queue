{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}
module Main where

import Test.Hspec
import Control.Concurrent.STM
import qualified Control.Concurrent.STM.TQueue as TQueue
import Control.Monad
import Data.Queue
import Control.Concurrent.Async (race)
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
    it "enqueues and tries to dequeue a message" do
      msg <- atomically do
        q <- newQueue
        enqueue q "Hello"
        tryDequeue q
      msg `shouldBe` Just "Hello"
    it "Nothing can be dequeued from an empty queue" do
      msg <- atomically do
        q <- newQueue @String
        tryDequeue q
      msg `shouldBe` Nothing
    it "peeks at the top element" do
      msg <- atomically do
        q <- newQueue
        enqueue q "Hello"
        peek q
      msg `shouldBe` "Hello"
    it "tries to peek at the top element" do
      msg <- atomically do
        q <- newQueue
        enqueue q "Hello"
        tryPeek q
      msg `shouldBe` Just "Hello"
    it "enqueues and dequeues many messages" do
      msgs <- atomically do
        q <- newQueue @Int
        forM_ [1..10] (enqueue q)
        forM [1..10] (const $ dequeue q)
      msgs `shouldBe` [1..10]
    it "behaves faster than TQueue in its worst case" do
      (q, q') <- atomically do
        q <- newQueue @Int
        q' <- TQueue.newTQueue @Int
        forM_ [1..1000] (TQueue.writeTQueue q')
        forM_ [1..1000] (enqueue q)
        pure (q, q')
      -- Nondeterministic tests hurt my soul, but its made up for by the
      -- warmth this test succeeding gives me.
      race (atomically (TQueue.readTQueue q'))
           (atomically (dequeue q))
        `shouldReturn` Right 1
    it "all reads should block on an empty queue" do
      q <- atomically (newQueue @Int)
      let seconds n = n * 1000000
      timeout (1 `seconds`) (atomically (dequeue q))
        `shouldReturn` Nothing
      timeout (1 `seconds`) (atomically (peek q))
        `shouldReturn` Nothing
    it "flushes everything properly" do
      msgs <- atomically do
        q <- newQueue @Int
        forM_ [1..100] (enqueue q)
        forM [1..100] (const $ dequeue q)
      msgs `shouldBe` [1..100]

