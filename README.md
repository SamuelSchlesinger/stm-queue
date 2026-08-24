# Concurrent Real-Time Queue

[![Hackage](https://img.shields.io/hackage/v/stm-queue.svg)](https://hackage.haskell.org/package/stm-queue)
[![Haskell CI](https://github.com/SamuelSchlesinger/stm-queue/actions/workflows/ci.yml/badge.svg)](https://github.com/SamuelSchlesinger/stm-queue/actions/workflows/ci.yml)

This is an implementation of an Okasaki-style real-time queue using STM. Its
incremental rotations avoid the latency spikes caused by reversing a large
rear list in one queue operation. This is an algorithmic real-time guarantee;
GHC and STM do not provide hard wall-clock real-time scheduling. The queue has
lower throughput than `TQueue` in exchange for more predictable structural
work per operation.

An unbounded queue can be used like this:

```haskell
main :: IO ()
main = do
  q <- atomically do
    q <- newQueue
    forM_ [1..1000] (enqueue q)
    pure q
  consumer q

consumer :: Queue Int -> IO ()
consumer q = forever do
  i <- atomically (dequeue q)
  print i
```

When queue creation does not need to be part of a transaction, `newQueueIO`
avoids the overhead of `atomically newQueue`.

Bounded queues use the same incremental queue algorithm and add transactional
backpressure:

```haskell
bounded :: STM (Queue Message)
bounded = newBoundedQueue 1024

send :: Queue Message -> Message -> STM ()
send = enqueue -- retries while a bounded queue is full

trySend :: Queue Message -> Message -> STM Bool
trySend = tryEnqueue -- returns False instead of retrying
```

`newBoundedQueueIO` constructs a bounded queue directly in `IO`. Dequeueing or
flushing a bounded queue releases capacity atomically. A capacity of zero is
valid and creates a queue that is always full. Bounded queues track free
capacity as split read and write credits, as `TBQueue` does, so producers and
the consumer rarely write the same `TVar`; unbounded queues retain the
original two-`TVar` representation and fast paths.

It also supports `peek`, which looks at the next element of the `Queue`.
`tryPeek`, `tryDequeue`, and `tryEnqueue` provide non-blocking variants of the
operations which can otherwise `retry`.

The package does not currently provide an equivalent of `unGetTQueue`.
