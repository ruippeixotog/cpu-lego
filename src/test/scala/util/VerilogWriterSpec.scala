package util

import java.io.IOException
import java.nio.file.Files

import scala.sys.process.{Process, ProcessLogger}

import component.BuilderAPI.*
import component.binaryAdder
import core.*
import org.specs2.execute.Skipped
import org.specs2.mutable.Specification
import simulator.Netlist

class VerilogWriterSpec extends Specification {

  "VerilogWriter" should {

    "export a NAND gate as a continuous assign" in {
      val a = new Port
      val b = new Port
      val y = new Port
      val top = CompositeComponent(
        "top",
        Map("g" -> NAND(a, b, y)),
        Nil,
        Map(
          "a" -> (Some(Direction.Input), a),
          "b" -> (Some(Direction.Input), b),
          "y" -> (Some(Direction.Output), y)
        )
      )

      val v = VerilogWriter.write(Netlist(top), "inv")
      v must contain("module inv(")
      v must contain("input a")
      v must contain("output y")
      v must contain("assign y = ~(a & b);")
      v must contain("endmodule")
    }

    "export a switch as a tri-state assign" in {
      val in = new Port
      val en = new Port
      val y = new Port
      val top = CompositeComponent(
        "top",
        Map("s" -> Switch(in, y, en)),
        Nil,
        Map(
          "in" -> (Some(Direction.Input), in),
          "en" -> (Some(Direction.Input), en),
          "y" -> (Some(Direction.Output), y)
        )
      )

      VerilogWriter.write(Netlist(top), "buf") must contain("assign y = en ? in : 1'bz;")
    }

    "exclude clocks, leaving a comment with the half period" in {
      val clk = new Port
      val top = CompositeComponent("top", Map("c" -> Clock(10, clk)), Nil, Map())

      val v = VerilogWriter.write(Netlist(top), "clkonly")
      v must contain("// clock with half period 10 ticks")
      v must not(contain("always"))
      v must not(contain("initial"))
    }

    "read the constant nets as literals" in {
      val a = new Port
      val b = new Port
      val y = new Port
      val top = CompositeComponent(
        "top",
        Map("g" -> NAND(a, b, y)),
        List((a, High), (b, Low)),
        Map("y" -> (Some(Direction.Output), y))
      )

      VerilogWriter.write(Netlist(top), "consts") must contain("assign y = ~(1'b1 & 1'b0);")
    }

    "infer directions for pins registered without one" in {
      val a = new Port
      val b = new Port
      val y = new Port
      val top = CompositeComponent(
        "top",
        Map("g" -> NAND(a, b, y)),
        Nil,
        Map(
          "a" -> (Option.empty[Direction], a),
          "b" -> (Option.empty[Direction], b),
          "y" -> (Option.empty[Direction], y)
        )
      )

      val v = VerilogWriter.write(Netlist(top), "inferred")
      v must contain("input a")
      v must contain("output y")
    }

    "export a registered input pin whose net is driven inside the design as inout" in {
      // The accumulator-on-a-bus case: the switch drives the bus net, but the pin is registered as an
      // input. Assigning to an `input` port is illegal, so the port must be `inout`.
      val in = new Port
      val en = new Port
      val bus = new Port
      val top = CompositeComponent(
        "top",
        Map("s" -> Switch(in, bus, en)),
        Nil,
        Map(
          "in" -> (Some(Direction.Input), in),
          "en" -> (Some(Direction.Input), en),
          "bus" -> (Some(Direction.Input), bus)
        )
      )

      val v = VerilogWriter.write(Netlist(top), "sharedbus")
      v must contain("inout bus")
      v must contain("assign bus = en ? in : 1'bz;")
    }

    "infer a pin on a clock's net as an input, since the clock is not exported" in {
      val clk = new Port
      val top = CompositeComponent(
        "top",
        Map("c" -> Clock(5, clk)),
        Nil,
        Map("clk" -> (Option.empty[Direction], clk))
      )

      val v = VerilogWriter.write(Netlist(top), "clkdrv")
      v must contain("input clk")
      v must contain("// clock with half period 5 ticks driving clk")
    }

    "escape hierarchical names that are not legal Verilog identifiers" in {
      val x = new Port
      val b = new Port
      val y = new Port
      val child = CompositeComponent(
        "child",
        Map("g" -> NAND(x, new Port, new Port)),
        Nil,
        Map("data" -> (Option.empty[Direction], Vector(x, new Port)))
      )
      val top = CompositeComponent(
        "top",
        Map("c" -> child, "g" -> NAND(x, b, y)),
        Nil,
        Map(
          "b" -> (Some(Direction.Input), b),
          "y" -> (Some(Direction.Output), y)
        )
      )

      val v = VerilogWriter.write(Netlist(top), "esc")
      v must contain("""wire \c.data[0] ;""")
      v must contain("""assign y = ~(\c.data[0]  & b);""")
    }

    "escape SystemVerilog keywords in top-level pin and module names" in {
      val a = new Port
      val b = new Port
      val y = new Port
      val top = CompositeComponent(
        "top",
        Map("g" -> NAND(a, b, y)),
        Nil,
        Map(
          "logic" -> (Some(Direction.Input), a),
          "b" -> (Some(Direction.Input), b),
          "y" -> (Some(Direction.Output), y)
        )
      )

      val v = VerilogWriter.write(Netlist(top), "int")
      v must contain("""module \int (""")
      v must contain("""input \logic """)
      v must contain("""assign y = ~(\logic  & b);""")
    }

    "collapse bus pins back into vector ports" in {
      val a = Vector(new Port, new Port, new Port, new Port)
      val s = Vector(new Port, new Port, new Port, new Port)
      val gates = a.indices.map(i => s"g$i" -> NAND(a(i), a(i), s(i))).toMap
      val top = CompositeComponent(
        "top",
        gates,
        Nil,
        Map(
          "a" -> (Some(Direction.Input), a),
          "s" -> (Some(Direction.Output), s)
        )
      )

      val v = VerilogWriter.write(Netlist(top), "bus")
      v must contain("input [3:0] a")
      v must contain("output [3:0] s")
      v must contain("assign s[0] = ~(a[0] & a[0]);")
      v must contain("assign s[3] = ~(a[3] & a[3]);")
    }

    "tie a secondary output pin to the primary pin of its net" in {
      // p (input) and q (output) are two pins on the same port: a feedthrough. The export reads
      // the net through p and ties q to it.
      val t = new Port
      val b = new Port
      val r = new Port
      val top = CompositeComponent(
        "top",
        Map("g" -> NAND(t, b, r)),
        Nil,
        Map(
          "p" -> (Some(Direction.Input), t),
          "q" -> (Some(Direction.Output), t),
          "b" -> (Some(Direction.Input), b),
          "r" -> (Some(Direction.Output), r)
        )
      )

      val v = VerilogWriter.write(Netlist(top), "tied")
      v must contain("assign r = ~(p & b);")
      v must contain("assign q = p;")
    }

    "prove a 16-bit adder equivalent to a behavioural model with yosys" in {
      if (!yosysAvailable) Skipped("yosys is not on the PATH")
      else {
        val dir = Files.createTempDirectory("verilog-writer-spec")
        val gate = VerilogWriter.write(Netlist(adder16), "adder")
        gate must contain("input [15:0] a")
        gate must contain("output [15:0] s")
        Files.writeString(dir.resolve("adder.v"), gate)
        Files.writeString(
          dir.resolve("adder_ref.v"),
          """module adder_ref(input [15:0] a, input [15:0] b, input cin, output [15:0] s, output cout);
            |  assign {cout, s} = a + b + cin;
            |endmodule
            |""".stripMargin
        )

        val log = new StringBuilder
        val logger = ProcessLogger(line => log.append(line).append('\n'), line => log.append(line).append('\n'))
        val script =
          "read_verilog adder.v; read_verilog adder_ref.v; " +
            "equiv_make adder_ref adder equiv; hierarchy -top equiv; " +
            "equiv_simple; equiv_status -assert"
        val exit = Process(Seq("yosys", "-p", script), dir.toFile).!(logger)
        if (exit != 0) failure("yosys equivalence proof failed:\n" + log.toString)
        else log.toString must contain("proven")
      }
    }
  }

  // A 16-bit adder built from the DSL's binaryAdder, with its buses registered as the module pins.
  private def adder16: Component = {
    val (_, comp) = buildComponent {
      val a = Vector.fill(16)(new Port)
      val b = Vector.fill(16)(new Port)
      val cin = new Port
      val (s, cout) = binaryAdder(a, b, cin)
      val env = summon[BuilderEnv]
      env.register("a", a, Some(Direction.Input))
      env.register("b", b, Some(Direction.Input))
      env.register("cin", cin, Some(Direction.Input))
      env.register("s", s, Some(Direction.Output))
      env.register("cout", cout, Some(Direction.Output))
    }
    comp
  }

  private def yosysAvailable: Boolean =
    try {
      Process(Seq("yosys", "-V")).!(ProcessLogger(_ => ())) == 0
    } catch {
      case _: IOException => false
    }
}
