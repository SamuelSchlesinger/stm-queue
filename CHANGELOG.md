# Revision history for stm-queue

## 0.2.2.0 -- UNRELEASED

- Bounded queues now track free capacity as split read and write credits, in
  the style of `TBQueue`. This batches capacity transfers when the workload
  allows; saturated queues may still transfer credits on every enqueue.
- Moved the implementation to a private `Data.Queue.Internal` module so
  property tests can inspect its representation without exposing constructors.
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
