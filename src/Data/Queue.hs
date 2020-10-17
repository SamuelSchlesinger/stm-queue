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
, peek
, tryPeek
, enqueue
, dequeue
, tryDequeue
, flush
) where

import Control.Concurrent
import Control.Concurrent.STM

-- | Real time 'Queue' backed by transactional variables ('TVar's)
data Queue a = Queue
  {-# UNPACK #-} !(TVar ([a], [a]))
  {-# UNPACK #-} !(TVar ([a], [a]))

-- | Create a new, empty 'Queue'
newQueue :: STM (Queue a)
newQueue = Queue
  <$> newTVar ([], [])
  <*> newTVar ([], [])

rotate :: [a] -> [a] -> [a]
rotate xs ys = go xs ys []
  where
  go [] bottom acc = bottom ++ acc
  go (t:ts) (b:bs) acc = t : go ts bs (b:acc)
  go ts [] acc = ts ++ acc

-- | Enqueue a single item onto the 'Queue'.
enqueue :: Queue a -> a -> STM ()
enqueue q@(Queue top bottom) a = do
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

-- | Dequeue a single item onto the 'Queue', 'retry'ing if there is nothing
-- there. This is the motivating use case of this library, allowing a thread to
-- register its interest in the head of a 'Queue' and be woken up by the
-- runtime system to read from the top of that 'Queue' when an item has
-- been made available.
dequeue :: Queue a -> STM a
dequeue q@(Queue top bottom) = do
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
tryDequeue q@(Queue top bottom) = do
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

-- | Peek at the top of the 'Queue', returning the top element.
peek :: Queue a -> STM a
peek (Queue top _bottom) =
  readTVar top >>= \case
    (x : xs, _) -> pure x
    ([], _) -> retry

-- | Try to 'peek' for the top item of the 'Queue'. This function is
-- offered to easily port from the 'TQueue' offered in the stm package,
-- but is not the intended usage of the library.
tryPeek :: Queue a -> STM (Maybe a)
tryPeek (Queue top _bottom) =
  readTVar top >>= \case
    (x : xs, _) -> pure (Just x)
    ([], _) -> pure Nothing  

-- | Efficiently read the entire contents of a 'Queue' into a list.
flush :: Queue a -> STM [a]
flush (Queue top bottom) = do
  (xs, _) <- swapTVar top ([], [])
  (ys, _) <- swapTVar bottom ([], [])
  pure (rotate xs ys)
