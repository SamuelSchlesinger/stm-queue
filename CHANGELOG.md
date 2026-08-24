# Revision history for stm-queue

## 0.2.2.0 -- 2026-08-23

- Bounded queues now track free capacity as split read and write credits, in
  the style of `TBQueue`. Producers and the consumer conflict on capacity
  accounting once per `limit` enqueues instead of on every operation, which
  is several times faster for a mailbox fed by many producers.
- Added `Data.Queue.Internal`, exposing the representation and its
  invariants for property tests. It is not covered by the PVP.
- Added a model-based property test and a multi-producer stress test.
- Added a bounded `Queue` versus `TBQueue` throughput benchmark
  (`--throughput-bounded`).

## 0.2.1.0 -- 2026-08-05

- Added bounded real-time queues with `newBoundedQueue` and
  `newBoundedQueueIO`.
- Added transactional backpressure to `enqueue`, plus non-blocking
  `tryEnqueue` and `isFull`.
- Preserved the original two-`TVar` fast paths for unbounded queues.
- Added `newQueueIO` for faster queue creation outside transactions.
- Avoided rebuilding the front list when an incremental rotation has an empty
  rear list.
- Repaired and expanded the latency, throughput, and creation benchmarks.
- Added CI coverage for GHC 9.6.7 through GHC 9.14.1.

## 0.2.0.0 -- 2023-01-17

- Relaxed the `base` upper bound for newer GHC releases.

## 0.1.0.0 -- 2020-09-19

Created a basic Okasaki-style real-time queue
