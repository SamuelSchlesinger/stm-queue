# Concurrent Real-Time Queue

[![Hackage](https://img.shields.io/hackage/v/stm-queue.svg)](https://hackage.haskell.org/package/stm-queue)
[![Build Status](https://travis-ci.org/SamuelSchlesinger/stm-queue.svg?branch=master)](https://travis-ci.org/SamuelSchlesinger/stm-queue)

This is an implementation of a real-time queue using STM. It has lower
throughput than `TQueue` but much lower latency as well. It does not support
an equivalent to `unGetTQueue`, but could be made to likely if someone needs it. Here's an
example:

```haskell
main :: IO ()
main = do
  q <- atomically do
    q <- atomically newQueue
    forM_ [1..1000] (enqueue q)
  consumer q

consumer :: Queue Int -> IO ()
consumer q = forever do
  i <- atomically (dequeue q)
  print i
```

It also supports `peek`, which looks at the next element of the `Queue`.
For each operation except for `enqueue`, there is a `try` prefixed version
which does not do an `stm` `retry` upon failure.
