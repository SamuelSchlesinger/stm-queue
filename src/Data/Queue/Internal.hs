{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{- |
Module: Data.Queue.Internal
Description: Representation and invariants of the real-time queue
Copyright: (c) Samuel Schlesinger 2020
License: MIT
Maintainer: sgschlesinger@gmail.com
Stability: experimental
Portability: POSIX, Windows

This private module contains the queue representation and its invariants.
Property tests compile it directly; the public API is "Data.Queue".
-}
module Data.Queue.Internal
( Queue (..)
, Capacity (..)
, newQueue
, newQueueIO
, newBoundedQueue
, newBoundedQueueIO
, peek
, tryPeek
, enqueue
, tryEnqueue
, dequeue
, tryDequeue
, isFull
, flush
, rotate
) where

import Control.Concurrent.STM
import Numeric.Natural (Natural)

-- | Real-time t'Queue' backed by transactional t'TVar' values.
-- Rotations are evaluated incrementally across queue operations.
--
-- If the two TVars contain @(ts, sts)@ and @(bs, sbs)@, respectively,
-- the logical contents are @ts ++ reverse bs@. In every stored state,
-- @length bs <= length ts@, and @ts@ is empty exactly when the queue is
-- empty. The two schedules are independent cursors into the current
-- incremental rotation, allowing the common enqueue and dequeue paths to
-- update different TVars. Each fast path advances its cursor by two cells;
-- together, they trigger a new rotation before the length invariant can be
-- violated.
--
-- For a bounded queue, the read and write credits sum to the free capacity:
-- @readCredits + writeCredits == limit - length (ts ++ reverse bs)@.
data Queue a = Queue
  !Capacity
  {-# UNPACK #-} !(TVar ([a], [a]))
  {-# UNPACK #-} !(TVar ([a], [a]))

-- | Unbounded queues deliberately carry no occupancy state, preserving the
-- two-TVar fast paths. Bounded queues track free capacity as two pools of
-- credits, in the style of 'Control.Concurrent.STM.TBQueue':
--
-- * 'enqueue' spends a write credit;
-- * 'dequeue' and 'flush' release read credits;
-- * only when the write credits are exhausted does an enqueue move the
--   accumulated read credits over, touching the TVar the consumer writes.
--
-- This can batch capacity transfers between producers and consumers. The
-- batch size depends on the workload: if a full queue is repeatedly dequeued
-- and immediately refilled, each enqueue transfers a single read credit.
data Capacity
  = Unbounded
  | Bounded
      !Natural                        -- ^ limit
      {-# UNPACK #-} !(TVar Natural)  -- ^ read credits, released by consumers
      {-# UNPACK #-} !(TVar Natural)  -- ^ write credits, spent by producers

-- | Create a new, empty t'Queue'.
newQueue :: STM (Queue a)
newQueue = Queue Unbounded
  <$> newTVar ([], [])
  <*> newTVar ([], [])

-- | Create a new, empty t'Queue' directly in 'IO'.
--
-- This avoids the overhead of @atomically newQueue@ when the queue does not
-- need to be created as part of a larger transaction.
--
-- @since 0.2.1.0
newQueueIO :: IO (Queue a)
newQueueIO = Queue Unbounded
  <$> newTVarIO ([], [])
  <*> newTVarIO ([], [])

-- | Create a new, empty t'Queue' with the given maximum number of items.
-- 'enqueue' retries while the queue is full, providing transactional
-- backpressure. A capacity of zero creates a queue which is always full.
--
-- The incremental queue rotation is unchanged. Bounded queues add two
-- credit TVars so capacity changes remain atomic with queue operations
-- while separating producer and consumer accounting between credit transfers.
--
-- @since 0.2.1.0
newBoundedQueue :: Natural -> STM (Queue a)
newBoundedQueue limit = Queue
  <$> (Bounded limit <$> newTVar 0 <*> newTVar limit)
  <*> newTVar ([], [])
  <*> newTVar ([], [])

-- | Create a new, empty bounded t'Queue' directly in 'IO'.
--
-- @since 0.2.1.0
newBoundedQueueIO :: Natural -> IO (Queue a)
newBoundedQueueIO limit = Queue
  <$> (Bounded limit <$> newTVarIO 0 <*> newTVarIO limit)
  <*> newTVarIO ([], [])
  <*> newTVarIO ([], [])

-- | Incrementally produce @xs ++ reverse ys@. Queue operations call this only
-- when @length ys <= length xs + 1@, under which every cell of the result
-- costs a constant amount of work to force.
rotate :: [a] -> [a] -> [a]
rotate xs [] = xs
rotate xs ys = go xs ys []
  where
  go [] bottom acc = bottom ++ acc
  go (t:ts) (b:bs) acc = t : go ts bs (b:acc)
  go ts [] acc = ts ++ acc

-- | Enqueue a single item onto the t'Queue', retrying if a bounded queue is
-- full.
enqueue :: Queue a -> a -> STM ()
enqueue queue a = do
  reserveSlot queue
  enqueueUnconditionally queue a

-- | Try to enqueue a single item without retrying. Returns 'False' when a
-- bounded queue is full. It always succeeds for an unbounded queue.
--
-- @since 0.2.1.0
tryEnqueue :: Queue a -> a -> STM Bool
tryEnqueue queue a = do
  reserved <- tryReserveSlot queue
  if reserved
    then enqueueUnconditionally queue a >> pure True
    else pure False

reserveSlot :: Queue a -> STM ()
reserveSlot (Queue capacity _top _bottom) =
  case capacity of
    Unbounded -> pure ()
    Bounded _limit readCredits writeCredits -> do
      w <- readTVar writeCredits
      if w > 0
        then writeTVar writeCredits $! w - 1
        else do
          r <- readTVar readCredits
          check (r > 0)
          writeTVar readCredits 0
          writeTVar writeCredits $! r - 1

tryReserveSlot :: Queue a -> STM Bool
tryReserveSlot (Queue capacity _top _bottom) =
  case capacity of
    Unbounded -> pure True
    Bounded _limit readCredits writeCredits -> do
      w <- readTVar writeCredits
      if w > 0
        then do
          writeTVar writeCredits $! w - 1
          pure True
        else do
          r <- readTVar readCredits
          if r > 0
            then do
              writeTVar readCredits 0
              writeTVar writeCredits $! r - 1
              pure True
            else pure False

releaseSlot :: Queue a -> STM ()
releaseSlot (Queue capacity _top _bottom) =
  case capacity of
    Unbounded -> pure ()
    Bounded _limit readCredits _writeCredits -> modifyTVar' readCredits (+ 1)

clearSlots :: Queue a -> STM ()
clearSlots (Queue capacity _top _bottom) =
  case capacity of
    Unbounded -> pure ()
    Bounded limit readCredits writeCredits -> do
      writeTVar readCredits 0
      writeTVar writeCredits limit

enqueueUnconditionally :: Queue a -> a -> STM ()
enqueueUnconditionally (Queue _capacity top bottom) a = do
  (bs, sbs) <- readTVar bottom
  let bs' = a : bs
  case sbs of
    _:_:sbs' -> do
      writeTVar bottom (bs', sbs')
    _ -> do
      (ts, _sts) <- readTVar top
      let !ts' = rotate ts bs'
      writeTVar bottom ([], ts')
      writeTVar top (ts', ts')

-- | Dequeue a single item from the t'Queue', 'retry'ing if there is nothing
-- there. This is the motivating use case of this library, allowing a thread to
-- register its interest in the head of a t'Queue' and be woken up by the
-- runtime system to read from the top of that t'Queue' when an item has
-- been made available.
dequeue :: Queue a -> STM a
dequeue queue@(Queue _capacity top bottom) = do
  (ts, sts) <- readTVar top
  case ts of
    [] -> retry
    t:ts' -> do
      result <- case sts of
        _:_:sts' -> do
          writeTVar top (ts', sts')
          pure t
        _ -> do
          (bs, _) <- readTVar bottom
          let !ts'' = rotate ts' bs
          writeTVar bottom ([], ts'')
          writeTVar top (ts'', ts'')
          pure t
      releaseSlot queue
      pure result

-- | Try to 'dequeue' a single item. This function is offered to allow
-- users to easily port from the 'TQueue' offered in the stm package,
-- but is not the intended usage of the library.
tryDequeue :: Queue a -> STM (Maybe a)
tryDequeue queue@(Queue _capacity top bottom) = do
  (ts, sts) <- readTVar top
  case ts of
    [] -> pure Nothing
    t:ts' -> do
      result <- case sts of
        _:_:sts' -> do
          writeTVar top (ts', sts')
          pure (Just t)
        _ -> do
          (bs, _) <- readTVar bottom
          let !ts'' = rotate ts' bs
          writeTVar bottom ([], ts'')
          writeTVar top (ts'', ts'')
          pure (Just t)
      releaseSlot queue
      pure result

-- | Peek at the top of the t'Queue', returning the top element.
peek :: Queue a -> STM a
peek (Queue _capacity top _bottom) =
  readTVar top >>= \case
    (x : _, _) -> pure x
    ([], _) -> retry

-- | Try to 'peek' for the top item of the t'Queue'. This function is
-- offered to easily port from the 'TQueue' offered in the stm package,
-- but is not the intended usage of the library.
tryPeek :: Queue a -> STM (Maybe a)
tryPeek (Queue _capacity top _bottom) =
  readTVar top >>= \case
    (x : _, _) -> pure (Just x)
    ([], _) -> pure Nothing

-- | Test whether a bounded t'Queue' is full. An unbounded queue is never full.
--
-- @since 0.2.1.0
isFull :: Queue a -> STM Bool
isFull (Queue capacity _top _bottom) =
  case capacity of
    Unbounded -> pure False
    Bounded _limit readCredits writeCredits -> do
      w <- readTVar writeCredits
      if w > 0
        then pure False
        else (== 0) <$> readTVar readCredits

-- | Efficiently read the entire contents of a t'Queue' into a list. Flushing
-- a bounded queue makes all of its capacity available in the same transaction.
flush :: Queue a -> STM [a]
flush queue@(Queue _capacity top bottom) = do
  (xs, _) <- swapTVar top ([], [])
  (ys, _) <- swapTVar bottom ([], [])
  clearSlots queue
  pure (rotate xs ys)
