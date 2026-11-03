# Benchmark results

Optimisation PRs report their gain here. Update with `sbt "bench --write-results"`.

## Machine

```
OS: Linux amd64
CPU: AMD EPYC 9D25 126-Core Processor x4
Memory: 8124908 kB
JVM: 25.0.4.1 (Eclipse Adoptium)
```

## Results

| workload | iters | edges | wall s | edges/s | ns/edge |
|---|---|---|---|---|---|
| sap1 | 4 | 524 | 10.21 | 51 | 19487402.9 |

_Measured with `sbt bench` (2026-11-03)._
