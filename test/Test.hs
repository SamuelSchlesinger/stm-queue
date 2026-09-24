{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSize, modifyMaxSuccess, prop)
import Test.QuickCheck
import Control.Concurrent.Async (concurrently, mapConcurrently_)
import Control.Concurrent.STM
import Control.Monad
import Data.Foldable (for_)
import Data.Queue.Internal
import Numeric.Natural (Natural)
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
    it "reuses capacity released by the consumer once write credits run out" do
      -- Exercises the credit hand-over path: fill the queue, drain it, and
      -- fill it again without flushing.
      q <- atomically (newBoundedQueue @Int 3)
      atomically (forM_ [1..3] (enqueue q))
      atomically (isFull q) `shouldReturn` True
      atomically (replicateM 3 (dequeue q)) `shouldReturn` [1, 2, 3]
      atomically (isFull q) `shouldReturn` False
      atomically (forM_ [4..6] (enqueue q))
      atomically (isFull q) `shouldReturn` True
      atomically (tryEnqueue q 7) `shouldReturn` False
      atomically (flush q) `shouldReturn` [4, 5, 6]

  describe "Data.Queue model" $ modifyMaxSuccess (const 1000) $ modifyMaxSize (const 300) do
    prop "behaves like a FIFO list and maintains its invariants" $
      forAll capacityGen \capacity ->
        forAll (listOf arbitrary) \ops ->
          ioProperty (runModel capacity ops)

  describe "Data.Queue under contention" do
    it "delivers every message from many producers exactly once and in order" do
      let producers = 8 :: Int
          perProducer = 5000 :: Int
      for_ [Nothing, Just 16, Just 1] \capacity -> do
        q <- maybe newQueueIO newBoundedQueueIO capacity
        let produce p = for_ [1 .. perProducer] \i ->
              atomically (enqueue q (p, i))
        result <- timeout 10000000 $ concurrently
          (mapConcurrently_ produce [1 .. producers])
          (replicateM (producers * perProducer) (atomically (dequeue q)))
        consumed <- case result of
          Nothing -> expectationFailure "producer/consumer test timed out" >> fail "timeout"
          Just ((), messages) -> pure messages
        for_ [1 .. producers] \p ->
          [i | (p', i) <- consumed, p' == p] `shouldBe` [1 .. perProducer]
        atomically (tryDequeue q) `shouldReturn` Nothing
        atomically (isFull q) `shouldReturn` False

-- * Model-based testing

data Op
  = Enq Int
  | TryEnq Int
  | Deq
  | TryDeq
  | Peek
  | TryPeek
  | Flush
  | IsFull
  deriving Show

instance Arbitrary Op where
  arbitrary = frequency
    [ (5, Enq <$> arbitrary)
    , (3, TryEnq <$> arbitrary)
    , (5, pure Deq)
    , (3, pure TryDeq)
    , (1, pure Peek)
    , (1, pure TryPeek)
    , (1, pure Flush)
    , (1, pure IsFull)
    ]

data Outcome
  = RUnit
  | RBool Bool
  | RInt Int
  | RMaybe (Maybe Int)
  | RList [Int]
  | RRetry
  deriving (Eq, Show)

capacityGen :: Gen (Maybe Natural)
capacityGen = frequency
  [ (1, pure Nothing)
  , (3, Just . fromIntegral <$> choose (0 :: Int, 8))
  , (1, Just . fromIntegral <$> choose (9 :: Int, 200))
  ]

-- | The specification: a FIFO list with an optional capacity.
model :: Maybe Natural -> [Int] -> Op -> ([Int], Outcome)
model capacity xs op = case op of
  Enq x
    | full -> (xs, RRetry)
    | otherwise -> (xs ++ [x], RUnit)
  TryEnq x
    | full -> (xs, RBool False)
    | otherwise -> (xs ++ [x], RBool True)
  Deq -> case xs of
    [] -> ([], RRetry)
    y : ys -> (ys, RInt y)
  TryDeq -> case xs of
    [] -> ([], RMaybe Nothing)
    y : ys -> (ys, RMaybe (Just y))
  Peek -> case xs of
    [] -> ([], RRetry)
    y : _ -> (xs, RInt y)
  TryPeek -> case xs of
    [] -> ([], RMaybe Nothing)
    y : _ -> (xs, RMaybe (Just y))
  Flush -> ([], RList xs)
  IsFull -> (xs, RBool full)
  where
    full = maybe False (\limit -> fromIntegral (length xs) >= limit) capacity

-- | Run an operation against the implementation, observing 'retry' via
-- 'orElse'.
runOp :: Queue Int -> Op -> IO Outcome
runOp q op = atomically (act `orElse` pure RRetry)
  where
    act = case op of
      Enq x -> RUnit <$ enqueue q x
      TryEnq x -> RBool <$> tryEnqueue q x
      Deq -> RInt <$> dequeue q
      TryDeq -> RMaybe <$> tryDequeue q
      Peek -> RInt <$> peek q
      TryPeek -> RMaybe <$> tryPeek q
      Flush -> RList <$> flush q
      IsFull -> RBool <$> isFull q

-- | Check the representation invariants documented in "Data.Queue.Internal"
-- against the model's contents.
checkInvariants :: Maybe Natural -> Queue Int -> [Int] -> IO (Maybe String)
checkInvariants capacity (Queue c top bottom) expected = atomically do
  (ts, _) <- readTVar top
  (bs, _) <- readTVar bottom
  credits <- case c of
    Unbounded -> pure Nothing
    Bounded _ readCredits writeCredits ->
      Just <$> ((+) <$> readTVar readCredits <*> readTVar writeCredits)
  let contents = ts ++ reverse bs
  pure $ msum
    [ failIf (length bs > length ts)
        ("rear longer than front: " ++ show (ts, bs))
    , failIf (null ts /= null contents)
        "front list empty but queue is not"
    , failIf (contents /= expected)
        ("contents " ++ show contents ++ " differ from model " ++ show expected)
    , case (capacity, credits) of
        (Just limit, Just free)
          | free + fromIntegral (length expected) /= limit ->
              Just ("credits " ++ show free ++ " do not account for "
                    ++ show (length expected) ++ " of " ++ show limit)
        _ -> Nothing
    ]
  where
    failIf b msg = if b then Just msg else Nothing

runModel :: Maybe Natural -> [Op] -> IO Property
runModel capacity ops = do
  q <- maybe newQueueIO newBoundedQueueIO capacity
  let go _ [] = pure (property True)
      go xs (op : rest) = do
        let (xs', expected) = model capacity xs op
        actual <- runOp q op
        if actual /= expected
          then pure $ counterexample
            (show op ++ ": expected " ++ show expected ++ " but got " ++ show actual)
            False
          else do
            problem <- checkInvariants capacity q xs'
            case problem of
              Just msg -> pure (counterexample (show op ++ ": " ++ msg) False)
              Nothing -> go xs' rest
  go [] ops
