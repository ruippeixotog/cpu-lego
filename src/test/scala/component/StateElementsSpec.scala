package component

import scala.collection.mutable

import component.BuilderAPI.*
import component.i8080.*
import computer.sap1.SAP1
import core.*
import org.specs2.mutable.Specification
import simulator.{Netlist, StateElement, StateElements}

class StateElementsSpec extends Specification {

  // The next value of the output net `qNet` of a bistable element, given values for its input nets and the current
  // value of `qNet`. The element's truth table is expressed over its representative state net, which may be the
  // complementary output, so the state bit is translated in both directions.
  private def nextQ(el: StateElement, qNet: Int, inputs: Map[Int, Boolean], q: Boolean): Boolean = {
    val table = el.nextState.get
    val stateIn = if (el.stateNet.get == qNet) q else !q
    val stateOut = table(inputs, stateIn)
    if (el.stateNet.get == qNet) stateOut else !stateOut
  }

  private def analyse[A](spec: Spec[A]): (A, Netlist, StateElements) = {
    val (res, comp) = buildComponent(spec)
    val nl = Netlist(comp)
    (res, nl, StateElements(nl))
  }

  // Evaluates one net of a combinational cone (a single NAND driver per net) from values on the cone's leaf nets.
  private def evalNet(nl: Netlist, net: Int, leaves: Map[Int, Boolean]): Boolean = {
    val memo = mutable.Map[Int, Boolean]()
    def go(n: Int): Boolean =
      if (n == nl.lowNet) false
      else if (n == nl.highNet) true
      else {
        memo.getOrElseUpdate(
          n,
          leaves.get(n) match {
            case Some(v) => v
            case None =>
              val ref = nl.driversOf(n).head
              val i = Netlist.refIndex(ref)
              !(go(nl.nandIn1(i)) && go(nl.nandIn2(i)))
          }
        )
      }
    go(net)
  }

  private def ring(n: Int, enable: Option[Port]): Spec[Vector[Port]] = newSpec {
    val ps = Vector.fill(n)(newPort())
    ps.indices.foreach { i =>
      val in2 = if (i == 0) enable.getOrElse(High) else High
      val out = nand(ps((i + 1) % n), in2)
      out ~> ps(i)
    }
    ps
  }

  "StateElements on a nandLatch" should {

    "find a bistable with the expected set/reset/hold behaviour" in {
      val set, reset = newPort()
      val ((q, nq), nl, res) = analyse(nandLatch(set, reset))
      val setNet = nl.netOf(set)
      val resetNet = nl.netOf(reset)
      val qNet = nl.netOf(q)
      val nqNet = nl.netOf(nq)

      res.elements must haveSize(1)
      val el = res.bistables.head
      el.memberNets must beEqualTo(Set(qNet, nqNet))
      el.inputNets.toSet must beEqualTo(Set(setNet, resetNet))
      el.stateNet must beSome(beOneOf(qNet, nqNet))

      def next(s: Boolean, r: Boolean, q: Boolean): Boolean =
        nextQ(el, qNet, Map(setNet -> s, resetNet -> r), q)

      // active-low inputs: driving set low sets, driving reset low resets, both high holds
      next(s = false, r = true, q = false) must beTrue
      next(s = false, r = true, q = true) must beTrue
      next(s = true, r = false, q = false) must beFalse
      next(s = true, r = false, q = true) must beFalse
      next(s = true, r = true, q = false) must beFalse
      next(s = true, r = true, q = true) must beTrue
      // both low (the forbidden combination) has a single stable state with both outputs high; asserted on the
      // representative bit itself, since q is not the complement of the stored bit in that state
      el.nextState.get(Map(setNet -> false, resetNet -> false), false) must beTrue
      el.nextState.get(Map(setNet -> false, resetNet -> false), true) must beTrue
    }
  }

  "StateElements on a norLatch" should {

    "find a bistable with the expected set/reset/hold behaviour" in {
      val set, reset = newPort()
      val ((q, nq), nl, res) = analyse(norLatch(set, reset))
      val setNet = nl.netOf(set)
      val resetNet = nl.netOf(reset)
      val qNet = nl.netOf(q)

      res.elements must haveSize(1)
      val el = res.bistables.head
      // the NOR gates are built from NANDs, so the latch's SCC is the whole six-net ring; it characterises
      // directly as a single bistable whose state bit is carried by one of the two outputs
      el.memberNets must haveSize(6)
      el.memberNets must contain(qNet, nl.netOf(nq))
      el.stateNet must beSome(beOneOf(qNet, nl.netOf(nq)))
      // the ring's primitives read the outputs of the input inverters inside the NOR gates, so the element's
      // inputs are those two nets; the rows below compose through them
      el.inputNets must haveSize(2)
      el.inputNets must not(contain(beOneOf(setNet, resetNet)))

      def next(s: Boolean, r: Boolean, q: Boolean): Boolean = {
        val fanIn = el.inputNets.map { net =>
          net -> evalNet(nl, net, Map(setNet -> s, resetNet -> r))
        }.toMap
        nextQ(el, qNet, fanIn, q)
      }

      // active-high inputs: set high sets, reset high resets, both low holds
      next(s = true, r = false, q = false) must beTrue
      next(s = false, r = true, q = true) must beFalse
      next(s = false, r = false, q = false) must beFalse
      next(s = false, r = false, q = true) must beTrue
      // both high (the forbidden combination) has a single stable state with both outputs low; asserted on the
      // representative bit itself, since q is not the complement of the stored bit in that state
      val fanIn = el.inputNets.map { net =>
        net -> evalNet(nl, net, Map(setNet -> true, resetNet -> true))
      }.toMap
      el.nextState.get(fanIn, false) must beFalse
      el.nextState.get(fanIn, true) must beFalse
    }
  }

  "StateElements on a latchClocked" should {

    "find the inner NAND pair as a bistable over the gating outputs" in {
      val set, reset, clk = newPort()
      val ((q, _), nl, res) = analyse(latchClocked(set, reset, clk))
      val setNet = nl.netOf(set)
      val resetNet = nl.netOf(reset)
      val clkNet = nl.netOf(clk)
      val qNet = nl.netOf(q)

      res.elements must haveSize(1)
      val el = res.bistables.head
      el.memberNets must haveSize(2)
      // the core's inputs are the outputs of the set/reset gating logic, not the component's own inputs
      el.inputNets must haveSize(2)
      el.inputNets must not(contain(beOneOf(setNet, resetNet, clkNet)))

      // compose the core's next-state function with its combinational fan-in, evaluated here in the test, and
      // compare against latchClocked's truth table: it follows set/reset while clk is high and holds while low
      val leaves = Map(setNet -> false, resetNet -> false, clkNet -> false)
      foreach(for {
        s <- List(false, true)
        r <- List(false, true)
        c <- List(false, true)
        if !(s && r && c) // forbidden by latchClocked's contract
        q <- List(false, true)
      } yield (s, r, c, q)) { case (s, r, c, q) =>
        val fanIn = el.inputNets.map { net =>
          net -> evalNet(nl, net, leaves ++ Map(setNet -> s, resetNet -> r, clkNet -> c))
        }.toMap
        val composed = nextQ(el, qNet, fanIn, q)
        val expected =
          if (!c) q
          else if (s) true
          else if (r) false
          else q
        composed must beEqualTo(expected)
      }
    }
  }

  "StateElements on a dLatch" should {

    "find the master and slave latches as separate bistables" in {
      val in, clk = newPort()
      val (_, _, res) = analyse(dLatch(in, clk))
      res.elements must haveSize(2)
      res.bistables must haveSize(2)
      res.passes must haveSize(2)
      res.passes.last.residualSccs must beEqualTo(0)
    }
  }

  "StateElements on a ring of three NAND gates" should {

    "classify it as an oscillator" in {
      val (ps, nl, res) = analyse(ring(3, None))
      res.elements must haveSize(1)
      val el = res.elements.head
      el.kind must beEqualTo(StateElement.Kind.Oscillator)
      el.memberNets must beEqualTo(ps.map(nl.netOf).toSet)
    }

    "classify it as an oscillator even with an enable input" in {
      val en = newPort()
      val (_, _, res) = analyse(ring(3, Some(en)))
      res.elements must haveSize(1)
      res.elements.head.kind must beEqualTo(StateElement.Kind.Oscillator)
    }
  }

  "StateElements on large graphs" should {

    "handle a wide ring iteratively and report it unsupported" in {
      val (_, _, res) = analyse(ring(1024, None))
      res.elements must haveSize(1)
      val el = res.elements.head
      el.kind must beEqualTo(StateElement.Kind.Unsupported)
      el.memberNets must haveSize(1024)
    }

    "handle a long acyclic chain feeding a latch" in {
      val chain: Spec[Port] = newSpec {
        var prev: Port = High
        for (i <- 0 until 2000) {
          val p = newPort()
          val out = nand(prev, High)
          out ~> p
          prev = p
        }
        nandLatch(prev, High)._1
      }
      val (_, _, res) = analyse(chain)
      res.bistables must haveSize(1)
      res.elements must haveSize(1)
    }
  }

  "StateElements on the SAP-1" should {

    "find only latch-level bistables, with no unsupported SCCs" in {
      val nl = Netlist(SAP1(Nil).comp)
      val res = StateElements(nl)
      res.unsupported must beEmpty
      res.oscillators must beEmpty
      res.bistables.size must beGreaterThan(100)
      res.bistables.count(_.memberNets.size == 2) must beGreaterThan(100)
      res.passes.size must beGreaterThan(1)
      res.passes.map(_.bistablesFound).sum must beEqualTo(res.bistables.size)
      res.passes.last.residualSccs must beEqualTo(0)
      res.report(nl) must contain("Pass 1:")
    }
  }

  "StateElements on the VM80A" should {

    "find only latch-level bistables, with no unsupported SCCs" in {
      val (_, comp) = buildComponent {
        val in = VM80Input()
        vm80a(in)
      }
      val nl = Netlist(comp)
      val res = StateElements(nl)
      res.unsupported must beEmpty
      res.oscillators must beEmpty
      res.bistables.size must beGreaterThan(100)
      res.passes.map(_.bistablesFound).sum must beEqualTo(res.bistables.size)
      res.passes.last.residualSccs must beEqualTo(0)
    }
  }
}
