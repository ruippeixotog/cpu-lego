# Benchmark results

Log of `master`'s performance, updated only by the repo owner from their machine on `master`
with `sbt "bench --write-results"`. Optimisation PRs never touch this file; they report base
vs head, measured back-to-back on the same machine, in the PR description instead.

| date | commit | workload | forks | median ns/edge | min ns/edge | max ns/edge | edges/s | JVM | CPU |
|---|---|---|---|---|---|---|---|---|---|
