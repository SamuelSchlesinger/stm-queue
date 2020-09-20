{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}
module Main where

import Test.Hspec
import Control.Concurrent.STM
import qualified Control.Concurrent.STM.TQueue as TQueue
import Control.Monad
import Data.Queue
import Control.Concurrent.Async (race)

main :: IO ()
main = hspec $ do
  describe "Data.Queue" do
    it "enqueues and dequeues a message" do
      msg <- atomically do
        q <- newQueue
        enqueue q "Hello"
        dequeue q
      msg `shouldBe` "Hello"
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
        forM_ [1..100] (TQueue.writeTQueue q')
        forM_ [1..100] (enqueue q)
        pure (q, q')
      -- Nondeterministic tests hurt my soul, but its made up for by the
      -- warmth this test succeeding gives me.
      race (atomically (TQueue.readTQueue q'))
           (atomically (dequeue q))
        `shouldReturn` Right 1
