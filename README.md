# CPU LEGO

[![CI](https://github.com/ruippeixotog/cpu-lego/actions/workflows/ci.yml/badge.svg)](https://github.com/ruippeixotog/cpu-lego/actions/workflows/ci.yml)

This is an implementation of a digital circuit simulator and the definition of ever-larger components up to SAP-1, [a primitive CPU](https://en.wikipedia.org/wiki/Simple-As-Possible_computer). The goal of this project was to learn (again) what the architecture of a computer looks like.

The SAP-1 built here is as described in [Digital Computer Electronics](https://dl.acm.org/doi/book/10.5555/573742), a book by Albert Paul Malvino.

## Requirements

- JDK 25 LTS (Temurin recommended).
- [sbt](https://www.scala-sbt.org/) 1.10.x.

Build and test with `sbt test`. Run the apps with `sbt "runMain computer.i8080.VM80aApp"` (VM80A) or `sbt "runMain computer.sap1.SAP1App"` (SAP-1); apps run in a forked JVM with generational ZGC enabled.

## Implementation

The goal of this project was to find out a minimal set of building blocks that could be used to construct higher and higher level components (e.g. from logical gates to adders to ALUs) up to a complete CPU - just like LEGOs. I was able to do that with the following three pieces:

- `NAND`: the universal logic gate, from which every other boolean function can be expressed.
- `Clock`: a [clock signal](https://en.wikipedia.org/wiki/Clock_signal) with a configurable frequency. This can also be reproduced using chains of NANDs given they have a non-zero propagation delay, but I decided to make them intrinsic as they are usually implemented outside the realm of digital circuits in the real world as well.
- `Switch`: a [tristate buffer](https://en.wikipedia.org/wiki/Three-state_logic), required to operate bidirectional shared buses.

Those are the only intrinsic components, implemented at the simulator level. Every other component is built as a function of these, with interactions between them simulated as digital circuits.

Memory needs no intrinsic primitive: the basic unit of storage is the [SR latch](https://en.wikipedia.org/wiki/Flip-flop_(electronics)#Simple_set-reset_latches) built from two NANDs wired in a loop, from which clocked latches (`latchClocked`), D flip-flops (`dLatch`) and JK flip-flops (`jkFlipFlop`) are composed as ordinary circuits. Because the simulator models pure transport delays with no noise, gated latches come with timing contracts: a latch whose gate falls within a few gate delays of a data change may oscillate instead of settling, hanging the simulation rather than corrupting data (see the contracts on `latchClocked` and `ram`).

The project is organized into the following packages:

- `core`: a package containing the core definitions needed for digital circuits, including definitions for the three components described above.
- `component`: the CPUs definitions and library of components used to build them, organized into different areas (e.g. logic, memory, arithmetic)
- `computer`: the classes needed to program and run computers.
- `simulator`: the implementation of the digital circuit simulator.
- `iss`: a cycle-exact NMOS 6502 instruction-set simulator in pure Scala — the golden model for the hand-written DSL 6502. It has no simulator dependency: it talks to memory through a per-cycle `Bus6502` trait (`read`/`write` per bus cycle), and `step()` runs a single bus cycle.
- `util`: small shared helpers, including `ExternalAssets` (see below).

It makes heavy use of two features introduced by Scala 3:

- [Context functions](https://docs.scala-lang.org/scala3/reference/contextual/context-functions.html): `Spec[A]` is an alias for a context function `BuilderEnv ?=> A` (a function that receives a given instance of a `BuilderEnv` and returns an `A`). This allowed me to hide away the mutable machinery needed to represent a component graph and expose only component blueprints fully focused on composition with zero boilerplate.

- [Metaprogramming](https://docs.scala-lang.org/scala3/reference/metaprogramming/index.html): `newSpec` is a macro used throughout component definitions as a way to define the boundaries of a logical component. It doesn't change the behavior of the code it wraps, but it collects information about the function's context (such as its name and name of their arguments) to allow for a better representation of the circuit at runtime (e.g. referencing ports by their name). `newPort` is another example of a macro-powered constructor.

## External test assets

Some tests need data files that are too large to vendor in the repo — e.g. the [SingleStepTests](https://github.com/SingleStepTests/65x02) 6502 suites used to validate the `iss` package cycle-by-cycle. `util.ExternalAssets` is the single place that knows where those files live:

- if `$CPU_LEGO_ASSETS/<name>` already exists it is used as-is, so a file you drop in by hand always wins;
- otherwise the file is fetched once from its URL and cached there.

The default cache directory is `~/.cache/cpu-lego`; set `$CPU_LEGO_ASSETS` to override it. When an asset cannot be obtained (no network, fetch failed), the tests that need it skip with a clear message instead of failing. In restricted environments, pre-populate the cache directory by hand (e.g. with curl) and the tests will pick the files up without any download.

The SingleStepTests suites run sampled by default (every 32nd test per opcode); set `CPU_LEGO_SINGLESTEP_FULL=1` to run the full set.

## License

Copyright (c) 2021-2022 Rui Gonçalves. See LICENSE for details.
