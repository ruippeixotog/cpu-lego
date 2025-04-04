package component.i8080

import java.nio.file.Path

import component.*
import component.BuilderAPI.*
import core.*
import yosys.ComponentCreator

import ControlBus.*
import ControlBus.Bit.*

case class VM80Input(
    clk: Port = new Port(),
    f1: Port = new Port(),
    f2: Port = new Port(),
    reset: Port = new Port(),
    d: Bus @inout = Vector.fill(8)(new Port()),
    hold: Port = new Port(),
    ready: Port = new Port(),
    int: Port = new Port()
) {
  require(d.length == 8, s"VM80A data bus must be 8 bits, but got ${d.length}")
}

case class VM80Output(a: Bus, hlda: Port, wait0: Port, inte: Port, sync: Port, dbin: Port, wr: Port) {
  require(a.length == 16, s"VM80A address bus must be 16 bits, but got ${a.length}")
}

val vm80aCreator = ComponentCreator.fromVerilog(Path.of("src/main/verilog/vm80a/org/rtl/vm80a.v"))

def vm80a(in: VM80Input): Spec[VM80Output] = {
  val out = vm80aCreator.create(
    "vm80a",
    Map(
      "pin_clk" -> in.clk,
      "pin_f1" -> in.f1,
      "pin_f2" -> in.f2,
      "pin_reset" -> in.reset,
      "pin_d" -> in.d,
      "pin_hold" -> in.hold,
      "pin_ready" -> in.ready,
      "pin_int" -> in.int
    )
  )
  VM80Output(
    a = out("pin_a").asInstanceOf[Bus],
    hlda = out("pin_hlda").asInstanceOf[Port],
    wait0 = out("pin_wait").asInstanceOf[Port],
    inte = out("pin_inte").asInstanceOf[Port],
    sync = out("pin_sync").asInstanceOf[Port],
    dbin = out("pin_dbin").asInstanceOf[Port],
    wr = out("pin_wr_n").asInstanceOf[Port]
  )
}
