{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{- |
Module: Data.Queue
Description: A real-time, concurrent, and mutable queue
Copyright: (c) Samuel Schlesinger 2020
License: MIT
Maintainer: sgschlesinger@gmail.com
Stability: experimental
Portability: POSIX, Windows
-}
module Data.Queue
( Queue
, newQueue
, newQueueIO
, peek
, tryPeek
, enqueue
, dequeue
, tryDequeue
, flush
) where

import Control.Concurrent.STM

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
data Queue a = Queue
  {-# UNPACK #-} !(TVar ([a], [a]))
  {-# UNPACK #-} !(TVar ([a], [a]))

-- | Create a new, empty t'Queue'.
newQueue :: STM (Queue a)
newQueue = Queue
  <$> newTVar ([], [])
  <*> newTVar ([], [])

-- | Create a new, empty t'Queue' directly in 'IO'.
--
-- This avoids the overhead of @atomically newQueue@ when the queue does not
-- need to be created as part of a larger transaction.
newQueueIO :: IO (Queue a)
newQueueIO = Queue
  <$> newTVarIO ([], [])
  <*> newTVarIO ([], [])

-- Queue operations call this only when @length ys <= length xs + 1@, under
-- which it incrementally produces @xs ++ reverse ys@.
rotate :: [a] -> [a] -> [a]
rotate xs [] = xs
rotate xs ys = go xs ys []
  where
  go [] bottom acc = bottom ++ acc
  go (t:ts) (b:bs) acc = t : go ts bs (b:acc)
  go ts [] acc = ts ++ acc

-- | Enqueue a single item onto the t'Queue'.
enqueue :: Queue a -> a -> STM ()
enqueue (Queue top bottom) a = do
  (bs, sbs) <- readTVar bottom
  let bs' = a : bs
  case sbs of
    _:_:sbs' -> do
      writeTVar bottom (bs', sbs')
    _ -> do
      (ts, _sts) <- readTVar top
      let ts' = rotate ts bs'
      writeTVar bottom ([], ts')
      writeTVar top (ts', ts')

-- | Dequeue a single item from the t'Queue', 'retry'ing if there is nothing
-- there. This is the motivating use case of this library, allowing a thread to
-- register its interest in the head of a t'Queue' and be woken up by the
-- runtime system to read from the top of that t'Queue' when an item has
-- been made available.
dequeue :: Queue a -> STM a
dequeue (Queue top bottom) = do
  (ts, sts) <- readTVar top
  case ts of
    [] -> retry
    t:ts' ->
      case sts of
        _:_:sts' -> do
          writeTVar top (ts', sts')
          pure t
        _ -> do
          (bs, _) <- readTVar bottom
          let !ts'' = rotate ts' bs
          writeTVar bottom ([], ts'')
          writeTVar top (ts'', ts'')
          pure t

-- | Try to 'dequeue' a single item. This function is offered to allow
-- users to easily port from the 'TQueue' offered in the stm package,
-- but is not the intended usage of the library.
tryDequeue :: Queue a -> STM (Maybe a)
tryDequeue (Queue top bottom) = do
  (ts, sts) <- readTVar top
  case ts of
    [] -> pure Nothing
    t:ts' ->
      case sts of
        _:_:sts' -> do
          writeTVar top (ts', sts')
          pure (Just t)
        _ -> do
          (bs, _) <- readTVar bottom
          let !ts'' = rotate ts' bs
          writeTVar bottom ([], ts'')
          writeTVar top (ts'', ts'')
          pure (Just t)

-- | Peek at the top of the t'Queue', returning the top element.
peek :: Queue a -> STM a
peek (Queue top _bottom) =
  readTVar top >>= \case
    (x : _, _) -> pure x
    ([], _) -> retry

-- | Try to 'peek' for the top item of the t'Queue'. This function is
-- offered to easily port from the 'TQueue' offered in the stm package,
-- but is not the intended usage of the library.
tryPeek :: Queue a -> STM (Maybe a)
tryPeek (Queue top _bottom) =
  readTVar top >>= \case
    (x : _, _) -> pure (Just x)
    ([], _) -> pure Nothing

-- | Efficiently read the entire contents of a t'Queue' into a list.
flush :: Queue a -> STM [a]
flush (Queue top bottom) = do
  (xs, _) <- swapTVar top ([], [])
  (ys, _) <- swapTVar bottom ([], [])
  pure (rotate xs ys)
