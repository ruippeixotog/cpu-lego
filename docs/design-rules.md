# Synchronous design rules

These rules keep gate-level designs timing-analysable in `RefSim`, parity-safe, and make the fast
simulator's flip-flop inference exact. They apply to every hand-written CPU in this repository.

## The rules

1. **All state lives in kit registers built on a `ClockDomain`.** The domain owns the single shared
   two-phase generator; every flip-flop in the domain is built on those two phases. Never build a
   private phase generator per bit — `dLatch` does that only for standalone use.
2. **No clock is derived from data, and there are no ripple counters.** Every storage element is
   clocked by the domain clock. Use `syncCounter`, never a counter whose stages are clocked by each
   other's outputs.
3. **Enables are sampled by the flip-flop (a load multiplexer). They never gate a clock.**
   `reg` implements `q' = en ? d : q` inside the flip-flop; gating the clock with combinational
   logic produces glitches that violate the flip-flop's timing contract.
4. **Asynchronous reset comes only from the reset pin, through a synchroniser.** The domain builds
   a two-flip-flop synchroniser on its `resetN` pin: assertion is asynchronous, release takes two
   clock edges, so every flip-flop sees the release on the same edge. Use it for power-on reset
   only — resets raised while the machine is running must use `reg`'s synchronous `syncReset`, which
   wins over the enable.
5. **No combinational loops outside kit storage elements.** Feedback is allowed only through the
   kit's flip-flops (`reg`, `syncCounter`, `shiftReg`), whose master-slave structure is the one
   analysed loop shape. Anything else that feeds back on itself is a lint error.
6. **Control lives in ROMs and PLAs where practical.** They are easy to review and the fast
   simulator tabulates them.

## Using the kit

```scala
val dom = clockDomain(clk, resetN) // resetN optional; High disables the async reset
given ClockDomain = dom

val r = reg(d, en, syncReset = dom.resetSync, resetValue = 0) // power-on reset
val c = syncCounter(8, en, load, loadValue = 0)
val s = shiftReg(8, in, en) // dir = High shifts toward the LSB
```

- `reg` captures on the rising edge of the domain clock. Constant `High`/`Low` enables and resets
  are folded away instead of built as gates.
- `syncCounter`'s `load` wins over counting and over `en`.
- The existing `register`/`counter` are *not* part of the kit: they predate these rules and stay
  untouched for SAP-1.

## Timing contracts

`d`, `en`, `syncReset`, `load` and `dir` must be stable around the rising clock edge, and the clock
must be slow enough for all signals to settle between edges. The asynchronous `resetN` may change at
any time, but assume nothing about the machine state except after it has been released for two full
clock edges.
