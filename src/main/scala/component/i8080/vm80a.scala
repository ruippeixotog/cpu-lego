package component.i8080

import java.nio.file.Path

import component.*
import component.BuilderAPI.*
import core.*
import yosys.ComponentCreator

import ControlBus.*
import ControlBus.Bit.*

// TODO fill in with interface
case class VM80Input()
case class VM80Output()

val vm80aCreator = ComponentCreator.fromVerilog(Path.of("src/main/verilog/vm80a/org/rtl/vm80a.v"))

def vm80a(in: VM80Input): Spec[VM80Output] = {
  val vm80a = vm80aCreator.create("vm80a", Map())
  VM80Output()
}
