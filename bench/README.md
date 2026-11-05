# Benchmarks

`sbt bench` runs the performance benchmark suite: every registered workload in
`bench.Workloads` is driven against the gate-level engine and the report shows
simulated throughput.

## Running

```
sbt bench
sbt "bench --workload sap1 --profile"
sbt "bench --write-results"
```

Options (`sbt "bench --help"`):

| flag | default | meaning |
|---|---|---|
| `--workload <name>` | all | only run this workload |
| `--forks <n>` | 5 (7 with `--write-results`) | fresh JVMs per workload, run sequentially |
| `--warmup-seconds <s>` | 3.0 | fixed warmup time per JVM |
| `--measure-seconds <s>` | 3.0 | measurement time per JVM; each JVM reports its median ns/edge across iterations |
| `--profile` | off | count signal transitions via `engine.watch()`; reports events per edge (adds observer overhead) |
| `--write-results` | off | append the full suite's rows to `bench/results.md` (only on `master` with a clean tree, on the `.sdkmanrc` JDK; cannot be combined with `--workload` or `--profile`) |

The primary metrics are **edges/s** (clock edges per wall-clock second) and
**ns/edge** (wall-clock nanoseconds per clock edge). `--profile` additionally
reports **events per edge** — signal transitions per clock edge, counted
black-box through the `Engine` interface.

## Reproducibility

A single JVM's number cannot be trusted to a few percent: separate JVMs on
one commit differ by 5–14%, with the offset fixed at JVM start, while
iterations inside one JVM vary by only 1.5–2%. So each workload runs in
several fresh JVMs that `Bench` launches itself (same `java`, same classpath,
G1 regardless of the ZGC setting the build keeps for the real-time frontend),
run sequentially so forks never contend for CPU. Each JVM warms up for
`--warmup-seconds`, then measures for `--measure-seconds` and reports its
median ns/edge across iterations. The report shows, per workload, the
**median across JVMs** and the **min–max range** — the range is the honest
error bar. Two consecutive `sbt bench` runs of one commit on a quiet machine
agree within 3% on the median; differences within about 2–3% at 5 JVMs are
noise, not a gain.

Base vs head: alternate single-fork runs between the two builds (A, B, A,
B…) with `sbt "bench --forks 1"` on each checkout, so drift hits both sides
equally, and compare the medians of the collected forks; or compare full
`--write-results`-style suites back-to-back on one machine and treat a
difference inside the reported range as noise.

## Tracked results

`bench/results.md` is a log of `master`'s performance, updated only by the
repo owner, from their machine, on `master`, with
`sbt "bench --write-results"` (7 forks per workload). Each run appends one row
per workload, and each row carries its own date, commit, fork count, median
ns/edge, min–max range, JVM and CPU, so rows from different machines stay
comparable in context. The command refuses to write unless HEAD is `master`,
the working tree is clean apart from `bench/results.md`, and the JDK major
matches the one `.sdkmanrc` pins, so every row maps to a commit anyone can
check out. Do not hand-edit the numbers.

Optimisation PRs never touch `bench/results.md`. Instead, the PR description
reports a base-vs-head comparison measured as above, with median ns/edge (and
range) for both and the ratio.

## Adding a workload

1. Implement `bench.Workload`:
   - `build(factory)` returns a `WorkloadRun` holding a fresh `Engine` built
     by Bench's factory.
   - `WorkloadRun.iterate()` drives exactly one iteration and returns the
     clock edges driven. It must be deterministic — repeated calls return
     the same edge count — and engine-native: use only the `Engine`
     interface (`set`/`get`/`runTo`/`step`/`watch`/`tick`), never simulator
     internals, so the workload measures any engine faithfully.
   - `WorkloadRun.ports` defaults to every port on the circuit's wires,
     deduplicated, for `--profile` transition counting.
2. Append the workload to `bench.Workloads.all`.
3. Add a spec in `src/test/scala/bench/` proving the driver is functionally
   correct (it must actually do what it claims — e.g. the SAP-1 workload runs
   the demo program to `HLT` with the expected output) and deterministic.

`bench.Sap1Workload` is the reference implementation: it mirrors the
wall-clock driver protocol of `computer.sap1.SAP1` with tick bounds instead of
wall-clock waits.

## Planned workloads

- **VM80A**: deferred to #116; lands once a memory device exists.
- **6502 / Apple IIe / PC**: add once those machines exist.
