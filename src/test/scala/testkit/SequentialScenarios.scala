package testkit

import core._
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Prop}
import org.specs2.ScalaCheck
import org.specs2.execute.{AsResult, Result}
import org.specs2.mutable.Specification
import simulator.{PortChange, Sim, SimState}

trait SequentialScenarios { this: Specification with ScalaCheck =>
  type BeforeActionFunc = (SimState, Port, Boolean, Option[Boolean]) => SimState
  type ActionFunc = (SimState, Port, Boolean, Option[Boolean]) => Unit

  case class SequentialScenario(
      comp: Component,
      ports: Seq[(Port, Option[Boolean])] = Nil,
      testCases: Seq[Seq[(Port, Boolean)]] = Nil,
      onStartFunc: SimState => Unit = _ => {},
      beforeActionFuncs: Seq[BeforeActionFunc] = Vector(),
      onActionFuncs: Seq[ActionFunc] = Vector(),
      checkFunc: SimState => Result = _ => ok
  ) {

    def withPorts(ports: Port | Vector[Port] | (Port, Boolean) | (Vector[Port], Boolean)*) = {
      val newPorts: Seq[(Port, Option[Boolean])] = ports.flatMap {
        case p: Port => Vector(p -> None)
        case bus: Vector[Port] @unchecked => bus.map(_ -> None)
        case (p: Port, v: Boolean) => Vector(p -> Some(v))
        case (bus: Vector[Port] @unchecked, v: Boolean) => bus.map(_ -> Some(v))
      }
      copy(ports = this.ports ++ newPorts)
    }

    def withTestCases(testCases: Seq[(Port, Boolean)]*) = copy(testCases = this.testCases ++ testCases)

    def onStart(f: SimState => Unit) = copy(onStartFunc = f)
    def beforeAction(f: BeforeActionFunc) = copy(beforeActionFuncs = beforeActionFuncs :+ f)
    def onAction(f: ActionFunc) = copy(onActionFuncs = onActionFuncs :+ f)

    def onPosEdge(port: Port)(f: SimState => Unit) = onAction {
      case (state, `port`, true, oldVal) if oldVal != Some(true) => f(state)
      case _ => // do nothing
    }
    def onNegEdge(port: Port)(f: SimState => Unit) = onAction {
      case (state, `port`, false, oldVal) if oldVal != Some(false) => f(state)
      case _ => // do nothing
    }
    def whenHigh(port: Port)(f: SimState => Unit) = onAction {
      case (state, _, _, _) if state.get(port) == Some(true) => f(state)
      case _ => // do nothing
    }
    def whenLow(port: Port)(f: SimState => Unit) = onAction {
      case (state, _, _, _) if state.get(port) == Some(false) => f(state)
      case _ => // do nothing
    }

    def check(f: SimState => Result) = copy(checkFunc = f)

    def run(): Prop = {
      given Arbitrary[Port] = Arbitrary(Gen.oneOf(ports.map(_._1)))

      testCases.foreach(runTestCase)
      forAll { (actions: List[(Port, Boolean)]) =>
        runTestCase(actions)
      }
    }

    def runTestCase(actions: Seq[(Port, Boolean)]): Result = {
      var state = Sim.setupAndRun(comp)
      ports.foreach { case (port, newVal) => state = state.schedule(0, PortChange(port, newVal)) }
      state = state.run()
      onStartFunc(state)

      foreach(actions) { case (port, newVal) =>
        val oldVal = state.get(port)
        beforeActionFuncs.foreach { f => state = f(state, port, newVal, oldVal) }

        state = state.schedule(0, PortChange(port, Some(newVal)))
        state = state.run()

        onActionFuncs.foreach(_(state, port, newVal, oldVal))
        checkFunc(state)
      }
    }
  }
}
