{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
module Data.Queue
( newQueue
, peek
, enqueue
, dequeue
) where

import Control.Concurrent
import Control.Concurrent.STM

-- | Real time queues in STM
data Queue a = Queue
  !(TVar [a])
  !(TVar [a])
  !(TVar [a])

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

enqueue :: Queue a -> a -> STM ()
enqueue q@(Queue _top _schedule bottom) a = do
  modifyTVar bottom (a :)
  queue q

dequeue :: Queue a -> STM a
dequeue q@(Queue top _schedule _bottom) =
  readTVar top >>= \case
    x : xs -> do
      writeTVar top xs
      queue q
      pure x
    [] -> retry

peek :: Queue a -> STM (Maybe a)
peek (Queue top _schedule _bottom) =
  readTVar top >>= \case
    x : xs -> pure (Just x)
    [] -> pure Nothing  
