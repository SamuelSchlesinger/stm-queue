# Revision history for stm-queue

## 0.2.0.0 -- 2026-08-05

- Added `newQueueIO` for faster queue creation outside transactions.
- Avoided rebuilding the front list when an incremental rotation has an empty
  rear list.
- Repaired and expanded the latency, throughput, and creation benchmarks.
- Added CI coverage for GHC 9.6.7 through GHC 9.14.1.

## 0.1.0.0 -- 2020-09-19

Created a basic Okasaki-style real-time queue
