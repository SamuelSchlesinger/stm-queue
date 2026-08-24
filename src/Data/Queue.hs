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
) where

import Data.Queue.Internal
