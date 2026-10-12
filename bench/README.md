# Benchmarks

`sbt bench` runs the performance benchmark suite: every registered workload in
`bench.Workloads` is driven against the gate-level engine and the report shows
simulated throughput.

## Running

```
sbt bench
sbt "bench --workload sap1 --profile"
sbt "bench --iterations 10 --write-results"
```

Options (`sbt "bench --help"`):

| flag | default | meaning |
|---|---|---|
| `--workload <name>` | all | only run this workload |
| `--warmup <n>` | 2 | unmeasured warmup iterations |
| `--iterations <n>` | time-based | fixed measured iteration count |
| `--min-seconds <s>` | 2.0 | minimum measured wall time per workload (at least 3 iterations) |
| `--profile` | off | count signal transitions via `engine.watch()`; reports events per tick (adds observer overhead) |
| `--write-results` | off | rewrite `bench/results.md` with this run |

The primary metrics are **ticks/s** (simulated ticks per wall-clock second),
**MHz** (simulated megahertz; 1 tick = 1 ns of simulated time) and **ns/tick**
(wall-clock nanoseconds per simulated tick). `--profile` additionally reports
**events per tick** — signal transitions per simulated tick, counted black-box
through the `Engine` interface.

## Tracked results

`bench/results.md` holds the baseline numbers plus machine info. Optimisation
PRs are expected to report their gain there: run
`sbt "bench --write-results"` on the PR's head and commit the updated file, or
paste the relevant rows into the PR description. Do not hand-edit the numbers.

## Adding a workload

1. Implement `bench.Workload`:
   - `build()` returns a `WorkloadRun` holding a fresh `Engine`.
   - `WorkloadRun.iterate()` drives exactly one iteration and returns the
     ticks advanced. It must be deterministic — repeated calls advance by the
     same tick count — and engine-native: use only the `Engine` interface
     (`set`/`get`/`runTo`/`step`/`watch`/`tick`), never simulator internals,
     so the workload measures any engine faithfully.
   - `WorkloadRun.ports` lists every port of the circuit for `--profile`
     transition counting.
2. Append the workload to `bench.Workloads.all`.
3. Add a spec in `src/test/scala/bench/` proving the driver is functionally
   correct (it must actually do what it claims — e.g. the SAP-1 workload runs
   the demo program to `HLT` with the expected output) and deterministic.

`bench.Sap1Workload` is the reference implementation: it mirrors the
wall-clock driver protocol of `computer.sap1.SAP1` with tick bounds instead of
wall-clock waits.

## Planned workloads

- **VM80A** (issue #47): needs the device API (#16) for an honest memory
  model; a random-memory stub would measure garbage execution and is not
  worth committing.
- **6502 / Apple IIe / PC**: add once those machines exist.
