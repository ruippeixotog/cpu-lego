package computer.sap1

import component.*
import component.BuilderAPI.*
import component.sap1.*
import component.sap1.ControlBus.Bit.*
import computer.sap1.Instr.*
import core.*
import simulator.*
import testkit.*
import util.Implicits.*

/** A RAM chip as a [[Device]]: a combinational-read memory that answers on the bus after a propagation delay. While
  * `we` is high it writes the data bus into the addressed word; while `ce` is high and `we` is low it drives the
  * addressed word onto the data bus; otherwise it releases the bus. Words with a floating address or data bit are never
  * written, and a floating address is never driven.
  */
class RamDevice(addr: Bus, data: Bus, we: Port, ce: Port) extends Device {
  private val mem = Array.fill(1 << addr.length)(0)

  def onAttach(ctx: DeviceContext): Unit = {
    ctx.onChange(addr*)(respond(ctx))
    ctx.onChange(data*)(respond(ctx))
    ctx.onChange(we)(respond(ctx))
    ctx.onChange(ce)(respond(ctx))
  }

  private def respond(ctx: DeviceContext): Unit =
    if (ctx.read(we) == Some(true)) {
      if (ctx.floatingMask(addr) == 0 && ctx.floatingMask(data) == 0)
        mem(ctx.readBits(addr).toInt) = ctx.readBits(data).toInt
      ctx.release(data, 1)
    } else if (ctx.read(ce) == Some(true)) {
      if (ctx.floatingMask(addr) == 0) ctx.driveBits(data, mem(ctx.readBits(addr).toInt))
      else ctx.release(data, 1)
    } else ctx.release(data, 1)
}

/** The SAP-1 with its gate-level RAM replaced by exposed RAM-side ports: the address bus, the system bus, and the RAM
  * chip-enable. The programmer's data bus joins the system bus group, so programming writes through the bus a RAM chip
  * would see. The program counter is disconnected from the bus while programming: it would otherwise drive the bus
  * against the programmer. Execution with programming off is unchanged.
  */
def sap1WithRamPorts(clkSig: Port, clr: Port, ramIn: Input): Spec[(Port, Bus, Bus, Bus, Port)] = newSpec {
  val bus = newBus(8)

  val instr = newBus(4)
  val hlt = andM(instr*)
  val clk = component.sap1.clock(clkSig, clr, hlt)
  val con = sequencer(instr, clk, clr)

  instrRegister(bus, con(Li), clk, clr, con(Ei)) ~> instr

  progCounter(bus, con(Cp), not(clk), clr, and(con(Ep), not(ramIn.prog)))

  val mOut = inputAndMar(bus, con(Lm), ramIn, clk)
  val ramCe = and(con(Ce), not(ramIn.prog))

  val aOut = accumulator(bus, clk, con(La), con(Ea))
  val bOut = register(bus, con(Lb), clk)
  alu(bus, aOut, bOut, con(Su), con(Eu))

  val oOut = register(bus, con(Lo), clk)
  (hlt, oOut, mOut, bus, ramCe)
}

class RamDeviceSap1Spec extends BaseSpec {

  // 10 + 14 + 18 - 20 = 22
  val prog: List[MemEntry] = List(
    LDA(9),
    ADD(10),
    ADD(11),
    SUB(12),
    OUT,
    HLT,
    Data(0),
    Data(0),
    Data(0),
    Data(10),
    Data(14),
    Data(18),
    Data(20)
  )

  "A SAP-1 with a RAM device" should {

    "run the demo program to out = 22" in {
      val (sim, out) = runDeviceSap1(prog)
      sim.get(out).sequence.map(_.toSignedInt) must beEqualTo(Some(22))
    }
  }

  /** Functional harness for the device SAP-1: program the RAM device and toggle the clock to quiescence until the
    * program halts. Mirrors the gate-level harness, with the RAM device attached from power-on.
    */
  private def runDeviceSap1(prog: List[MemEntry]): (GateProcessor, Bus) = {
    val ramIn = {
      val progP, write = newPort()
      val addr = newBus(4)
      val data = newBus(8)
      Input(progP, write, addr, data)
    }

    val clkSig, clr = newPort()
    val ((hlt, out, mOut, bus, ramCe), comp) = buildComponent { sap1WithRamPorts(clkSig, clr, ramIn) }

    val addrD = newBus(4)
    val dataD = newBus(8)
    val weD, ceD = newPort()
    val extraWires =
      (addrD.zip(mOut) ++ dataD.zip(bus) ++ dataD.zip(ramIn.data) ++ List(weD -> ramIn.write, ceD -> ramCe)).toList
    val ram = new RamDevice(addrD, dataD, weD, ceD)

    def load(sim: GateProcessor, prog: List[MemEntry], addr: Int = 0): GateProcessor =
      prog match {
        case entry :: rest =>
          load(
            sim
              .set(ramIn.prog, true)
              .set(ramIn.addr, addr.toBoolVec(4))
              .set(
                ramIn.data,
                entry match {
                  case instr: Instr => instr.repr
                  case Data(value) => value.toBoolVec(8)
                }
              )
              .run()
              .set(ramIn.write, true)
              .run()
              .set(ramIn.write, false)
              .run(),
            rest,
            addr + 1
          )
        case Nil =>
          // Release the programmer's data bus: its ports share the system bus group, so leaving them driven
          // would contend with the RAM device on every read during execution.
          ramIn.data.foldLeft(sim.set(ramIn.prog, false)) { (s, p) => s.unset(p) }.run()
      }

    var sim = load(
      GateProcessor
        .setup(comp, extraWires)
        .attach(ram)
        .set(clkSig, false)
        .set(clr, false)
        .run()
        .set(clr, true)
        .run(),
      prog
    )
    while (sim.get(hlt) != Some(true))
      sim = sim.toggle(clkSig).run().toggle(clkSig).run()
    (sim, out)
  }
}
