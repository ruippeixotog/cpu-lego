# Benchmark results

Tracked by hand: optimisation PRs report their gain here. Update with `sbt "bench --write-results"`.

## Machine

```
OS: Linux amd64
CPU: AMD EPYC 9D25 126-Core Processor x2
Memory: 8125612 kB
JVM: 21.0.12.1 (Eclipse Adoptium)
```

## Results

| workload | iters | ticks | wall s | ticks/s | MHz | ns/tick |
|---|---|---|---|---|---|---|
| sap1 | 3 | 17691 | 6.94 | 2548 | 0.0025 | 392500.0 |

_Measured with `sbt bench` (2026-10-12)._
