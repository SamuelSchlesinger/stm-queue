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
( newQueue
, peek
, tryPeek
, enqueue
, dequeue
, tryDequeue
) where

import Control.Concurrent
import Control.Concurrent.STM

-- | Real time queue backed by transactional variables ('TVar's)
data Queue a = Queue
  !(TVar [a])
  !(TVar [a])
  !(TVar [a])

-- | Create a new, empty queue
newQueue :: STM (Queue a)
newQueue = Queue
  <$> newTVar []
  <*> newTVar []
  <*> newTVar []

rotate :: [a] -> [a] -> [a] -> [a]
rotate [] (y : _) zs = y : zs
rotate (x : xs) (y : ys) zs = x : rotate xs ys (y : zs)

queue :: Queue a -> STM ()
queue (Queue top schedule bottom) =
  readTVar schedule >>= \case
    x : xs -> writeTVar schedule xs
    [] -> do
      xs <- readTVar top
      ys <- readTVar bottom
      let rs = rotate xs ys []
      writeTVar top rs
      writeTVar bottom []
      writeTVar schedule rs

-- | Enqueue a single item onto the queue.
enqueue :: Queue a -> a -> STM ()
enqueue q@(Queue _top _schedule bottom) a = do
  modifyTVar bottom (a :)
  queue q

-- | Dequeue a single item onto the queue, 'retry'ing if there is nothing
-- there. This is the motivating use case of this library, allowing a thread to
-- register its interest in the head of a queue and be woken up by the
-- runtime system to read from the top of that queue when an item has
-- been made available.
dequeue :: Queue a -> STM a
dequeue q@(Queue top _schedule _bottom) =
  readTVar top >>= \case
    x : xs -> do
      writeTVar top xs
      queue q
      pure x
    [] -> retry

-- | Try to 'dequeue' a single item. This function is offered to allow
-- users to easily port from the 'TQueue' offered in the stm package,
-- but is not the intended usage of the library.
tryDequeue :: Queue a -> STM (Maybe a)
tryDequeue q@(Queue top _schedule _bottom) =
  readTVar top >>= \case
    x : xs -> do
      writeTVar top xs
      queue q
      pure (Just x)
    [] -> pure Nothing

-- | Peek at the top of the queue, returning the top element.
peek :: Queue a -> STM a
peek (Queue top _schedule _bottom) =
  readTVar top >>= \case
    x : xs -> pure x
    [] -> retry

-- | Try to 'peek' for the top item of the queue. This function is
-- offered to easily port from the 'TQueue' offered in the stm package,
-- but is not the intended usage of the library.
tryPeek :: Queue a -> STM (Maybe a)
tryPeek (Queue top _schedule _bottom) =
  readTVar top >>= \case
    x : xs -> pure (Just x)
    [] -> pure Nothing  

-- | Efficiently read the entire contents of a queue into a list.
flush :: Queue a -> STM [a]
flush (Queue top schedule bottom) = do
  xs <- swapTVar top []
  ys <- swapTVar bottom []
  writeTVar schedule []
  pure (xs ++ reverse ys)
