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
| `--profile` | off | count signal transitions via `engine.watch()`; reports events per edge (adds observer overhead) |
| `--write-results` | off | merge this run's rows into `bench/results.md` |

The primary metrics are **edges/s** (clock edges per wall-clock second) and
**ns/edge** (wall-clock nanoseconds per clock edge). `--profile` additionally
reports **events per edge** — signal transitions per clock edge, counted
black-box through the `Engine` interface.

## Tracked results

`bench/results.md` holds the baseline numbers plus machine info. Optimisation
PRs are expected to report their gain there: run
`sbt "bench --write-results"` on the PR's head and commit the updated file, or
paste the relevant rows into the PR description. Do not hand-edit the numbers.

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

- **VM80A**: executing a small loop; the memory model is still open.
- **6502 / Apple IIe / PC**: add once those machines exist.
