# CPU LEGO

This is an implementation of a digital circuit simulator and the definition of ever-larger components up to SAP-1, [a primitive CPU](https://en.wikipedia.org/wiki/Simple-As-Possible_computer). The goal of this project was to learn (again) what the architecture of a computer looks like.

The SAP-1 built here is as described in [Digital Computer Electronics](https://dl.acm.org/doi/book/10.5555/573742), a book by Albert Paul Malvino.

## Implementation

The goal of this project was to find out a minimal set of building blocks that could be used to construct higher and higher level components (e.g. from logical gates to adders to ALUs) up to a complete CPU - just like LEGOs. I was able to do that with the following four pieces:

- `NAND`: the universal logic gate, from which every other boolean function can be expressed.
- `FlipFlop`: an [SR latch](https://en.wikipedia.org/wiki/Flip-flop_(electronics)#Simple_set-reset_latches), the basic unit of memory. It could theoretically be implemented using two NANDs, but I was finding it hard to avoid race conditions with my current simulator implementation. I may revisit this later.
- `Clock`: a [clock signal](https://en.wikipedia.org/wiki/Clock_signal) with a configurable frequency. This can also be reproduced using chains of NANDs, but I decided to make them intrinsic as they are usually implemented outside the realm of digital circuits in the real world as well.
- `Switch`: a [tristate buffer](https://en.wikipedia.org/wiki/Three-state_logic), required to operate bidirectional shared buses.

Those are the only intrinsic components, implemented at the simulator level. Every other component is built as a function of these, with interactions between them simulated as digital circuits.

The project is organized into the following packages:

- `core`: a package containing the core definitions needed for digital circuits, including definitions for the four components described above.
- `component`: the CPUs definitions and library of components used to build them, organized into different areas (e.g. logic, memory, arithmetic)
- `computer`: the classes needed to program and run computers. 
- `simulator`: the implementation of the digital circuit simulator.

It makes heavy use of two features introduced by Scala 3:

- [Context functions](https://docs.scala-lang.org/scala3/reference/contextual/context-functions.html): `Spec[A]` is an alias for a context function `BuilderEnv ?=> A` (a function that receives an implicit instance of a `BuilderEnv` and returns an `A`). This allowed me to hide away the mutable machinery needed to represent a component graph and expose only component blueprints fully focused on composition with zero boilerplate.

- [Macros](https://docs.scala-lang.org/scala3/reference/metaprogramming/index.html): `newSpec` is a macro used throughout component definitions as a way to define the boundaries of a logical component. It doesn't change the behavior of the code it wraps, but it collects information about the function's context (such as its name and name of their arguments) to allow for a better representation of the circuit at runtime (e.g. referencing ports by their name). `newPort` is another example of a macro-powered constructor.

## License

Copyright (c) 2021-2022 Rui Gonçalves. See LICENSE for details.
