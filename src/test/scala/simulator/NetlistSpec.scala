package simulator

import core.*
import org.specs2.mutable.Specification

class NetlistSpec extends Specification {

  "Netlist" should {

    "merge nets across hierarchy levels" in {
      // distinct ports joined by wires declared at different hierarchy levels land on one net
      val n1out = new Port
      val x = new Port
      val y = new Port
      val n2in1 = new Port
      val n1 = NAND(new Port, new Port, n1out)
      val n2 = NAND(n2in1, new Port, new Port)
      val inner1 = CompositeComponent("inner1", Map("g" -> n1), List((n1out, x)), Map())
      val inner2 = CompositeComponent("inner2", Map("g" -> n2), List((y, n2in1)), Map())
      val mid = CompositeComponent("mid", Map("a" -> inner1, "b" -> inner2), List((x, y)), Map())
      val top = CompositeComponent("top", Map("m" -> mid), Nil, Map())

      val nl = Netlist(top)
      val net = nl.netOf(n1out)
      (net >= 2) must beTrue
      nl.netOf(x) must beEqualTo(net)
      nl.netOf(y) must beEqualTo(net)
      nl.netOf(n2in1) must beEqualTo(net)
    }

    "reserve constant nets for High and Low" in {
      val p = new Port
      val q = new Port
      val top = CompositeComponent(
        "top",
        Map("g" -> NAND(p, q, new Port)),
        List((p, High), (q, Low)),
        Map()
      )
      val nl = Netlist(top)
      nl.highNet must beEqualTo(0)
      nl.lowNet must beEqualTo(1)
      nl.netOf(p) must beEqualTo(nl.highNet)
      nl.netOf(q) must beEqualTo(nl.lowNet)
      nl.netOf(High) must beEqualTo(nl.highNet)
      nl.netOf(Low) must beEqualTo(nl.lowNet)
    }

    "build driver and reader lists" in {
      val a = new Port
      val b = new Port
      val out = new Port
      val c = new Port
      val n1 = NAND(a, b, out)
      val n2 = NAND(out, c, new Port)
      val top = CompositeComponent("top", Map("g1" -> n1, "g2" -> n2), Nil, Map())

      val nl = Netlist(top)
      val outNet = nl.netOf(out)

      val drivers = nl.driversOf(outNet)
      drivers.length must beEqualTo(1)
      Netlist.refKind(drivers(0)) must beEqualTo(Netlist.NandKind)
      Netlist.refIndex(drivers(0)) must beEqualTo(0)
      nl.nandOut(Netlist.refIndex(drivers(0))) must beEqualTo(outNet)

      val readers = nl.readersOf(outNet)
      readers.length must beEqualTo(1)
      Netlist.refKind(readers(0)) must beEqualTo(Netlist.NandKind)
      Netlist.refIndex(readers(0)) must beEqualTo(1)

      // undriven input nets have no drivers
      nl.driversOf(nl.netOf(a)) must beEmpty

      // a switch enable is a reader; the switch output is a tri-state driver but not a reader
      val en = new Port
      val sw = Switch(a, new Port, en)
      val nl2 = Netlist(CompositeComponent("top", Map("s" -> sw), Nil, Map()))
      val enReaders = nl2.readersOf(nl2.netOf(en))
      enReaders.length must beEqualTo(1)
      Netlist.refKind(enReaders(0)) must beEqualTo(Netlist.SwitchKind)
      val swDrivers = nl2.driversOf(nl2.netOf(sw.out))
      swDrivers.length must beEqualTo(1)
      Netlist.refKind(swDrivers(0)) must beEqualTo(Netlist.SwitchKind)
      Netlist.refIndex(swDrivers(0)) must beEqualTo(0)
      nl2.switchOut(Netlist.refIndex(swDrivers(0))) must beEqualTo(nl2.netOf(sw.out))
      nl2.readersOf(nl2.netOf(sw.out)) must beEmpty
    }

    "record pin directions and hierarchical names" in {
      val in = new Port
      val bus = Vector(new Port, new Port)
      val out = new Port
      val childOut = new Port
      val child = CompositeComponent(
        "child",
        Map("g" -> NAND(in, bus(0), out)),
        Nil,
        Map("cout" -> (Some(Direction.Output), childOut))
      )
      val top = CompositeComponent(
        "top",
        Map("c" -> child),
        List((childOut, out)),
        Map(
          "in" -> (Some(Direction.Input), in),
          "data" -> (Some(Direction.Inout), bus),
          "out" -> (Some(Direction.Output), out)
        )
      )

      val nl = Netlist(top)
      val byName = nl.pins.map(p => p.name -> p).toMap
      byName("in") must beEqualTo(Netlist.Pin("in", Some(Direction.Input), nl.netOf(in)))
      byName("data[0]") must beEqualTo(Netlist.Pin("data[0]", Some(Direction.Inout), nl.netOf(bus(0))))
      byName("data[1]") must beEqualTo(Netlist.Pin("data[1]", Some(Direction.Inout), nl.netOf(bus(1))))
      byName("out") must beEqualTo(Netlist.Pin("out", Some(Direction.Output), nl.netOf(out)))

      // hierarchical names follow Index's scheme
      nl.nameOf(nl.netOf(in)) must beSome("in")
      nl.nameOf(nl.netOf(bus(1))) must beSome("data[1]")
      // the child's named port is namespaced under the child
      nl.names.keys must contain("c.cout")
      // every named port stays reachable, even when several merge into one net
      nl.names("c.cout") must beEqualTo(nl.netOf(childOut))
      nl.names("out") must beEqualTo(nl.netOf(out))
      nl.names("c.cout") must beEqualTo(nl.names("out"))
    }

    "keep Circuit's groupOf/portsOf consistent" in {
      val a = new Port
      val b = new Port
      val c = new Port
      val d = new Port
      val n1 = NAND(a, b, c)
      val n2 = NAND(c, d, new Port)
      val circuit = Circuit(List(n1, n2), List((a, High)))

      circuit.groupOf(a) must beEqualTo(circuit.groupOf(High))
      circuit.groupOf(a) must not(beEqualTo(circuit.groupOf(b)))
      // c is joined to the rest only through components: its own group, distinct from a's and b's
      circuit.groupOf(c) must not(beEqualTo(circuit.groupOf(a)))
      circuit.groupOf(c) must not(beEqualTo(circuit.groupOf(b)))
      circuit.portsOf(circuit.groupOf(c)) must beEqualTo(Set(c))

      val group = circuit.groupOf(a)
      circuit.portsOf(group) must contain(a, High)
      circuit.portsOf(group) must not(contain(b))

      // unknown ports form singleton groups
      val fresh = new Port
      circuit.groupOf(fresh) must beEqualTo(PortGroup(fresh))
      circuit.portsOf(PortGroup(fresh)) must beEqualTo(Set(fresh))
    }

    "preserve Circuit's component and wire lists" in {
      val a = new Port
      val b = new Port
      val n = NAND(a, b, new Port)
      val wires = List((a, High))
      val circuit = Circuit(List(n), wires)
      circuit.components must beEqualTo(List(n))
      circuit.wires must beEqualTo(wires)

      // from a hierarchy: same components and wires, original pairing
      val n2 = NAND(new Port, new Port, new Port)
      val inner = CompositeComponent("inner", Map("g" -> n2), List((a, b)), Map())
      val top = CompositeComponent("top", Map("i" -> inner, "g" -> n), Nil, Map())
      val c2 = Circuit(top)
      c2.components.toSet must beEqualTo(Set(n, n2))
      c2.components must haveSize(2)
      c2.wires must beEqualTo(List((a, b)))
    }

    "flatten 10^6 NANDs in linear time" in {
      // 1000 instances of a 1000-gate random combinational block, wired through a bus.
      // Times both 10^5 and 10^6 NANDs: flattening must scale linearly, so the ratio must stay
      // far below the 100x a quadratic implementation would show. Absolute times are
      // printed for the record; they depend on the machine.
      def design(instCount: Int, gatesPerInst: Int): Component = {
        val bus = Vector.fill(instCount)(new Port)
        val insts = (0 until instCount).map { i =>
          val in = new Port
          var prev = in
          val nands = Vector.newBuilder[(String, Component)]
          val wires = Vector.newBuilder[(Port, Port)]
          var j = 0
          while (j < gatesPerInst) {
            val a = new Port
            val b = new Port
            val out = new Port
            nands += ((s"g$j", NAND(a, b, out)))
            wires += ((prev, a))
            wires += ((prev, b))
            prev = out
            j += 1
          }
          wires += ((prev, bus(i)))
          CompositeComponent(s"inst$i", nands.result().toMap, wires.result().toList, Map())
        }
        CompositeComponent(
          "top",
          insts.zipWithIndex.map { case (c, i) => s"inst$i" -> c }.toMap,
          Nil,
          Map("out" -> (Option.empty[Direction], bus))
        )
      }

      def timeFlatten(nands: Int): Double = {
        val top = design(nands / 1000, 1000)
        val t0 = System.nanoTime()
        val nl = Netlist(top)
        val ms = (System.nanoTime() - t0) / 1e6
        println(
          f"flattened ${nl.stats.nands}%,d NANDs (${nl.stats.ports}%,d ports, ${nl.stats.nets}%,d nets) in $ms%.0f ms"
        )
        nl.stats.nands must beEqualTo(nands)
        ms
      }

      val t100k = timeFlatten(100000)
      val t1m = timeFlatten(1000000)
      println(f"scaling ratio (10x the gates): ${t1m / t100k}%.1fx")
      // Linear scaling => ~10x; quadratic => ~100x. Generous margin for machine noise.
      (t1m < 30 * t100k) must beTrue
      // Backstop against pathological blowup (quadratic would take hours here).
      (t1m < 120000.0) must beTrue
    }
  }
}
