package testkit

import core.*
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Prop}
import org.specs2.ScalaCheck
import org.specs2.execute.{AsResult, Result}
import org.specs2.mutable.Specification
import simulator.Sim

trait SequentialScenarios { this: Specification & ScalaCheck =>
  type BeforeActionFunc = (Sim, Port, Boolean, Option[Boolean]) => Sim
  type ActionFunc = (Sim, Port, Boolean, Option[Boolean]) => Unit

  case class SequentialScenario(
      comp: Component,
      ports: Seq[(Port, Option[Boolean])] = Nil,
      testCases: Seq[Seq[(Port, Boolean)]] = Nil,
      onStartFunc: Sim => Unit = _ => {},
      beforeActionFuncs: Seq[BeforeActionFunc] = Vector(),
      onActionFuncs: Seq[ActionFunc] = Vector(),
      checkFunc: Sim => Result = _ => ok
  ) {

    def withPorts(ports: Port | Bus | (Port, Boolean) | (Bus, Boolean)*) = {
      val newPorts: Seq[(Port, Option[Boolean])] = ports.flatMap {
        case p: Port => Vector(p -> None)
        case bus: Bus @unchecked => bus.map(_ -> None)
        case (p: Port, v: Boolean) => Vector(p -> Some(v))
        case (bus: Bus @unchecked, v: Boolean) => bus.map(_ -> Some(v))
      }
      copy(ports = this.ports ++ newPorts)
    }

    def withTestCases(testCases: Seq[(Port, Boolean)]*) = copy(testCases = this.testCases ++ testCases)

    def onStart(f: Sim => Unit) = copy(onStartFunc = f)
    def beforeAction(f: BeforeActionFunc) = copy(beforeActionFuncs = beforeActionFuncs :+ f)
    def onAction(f: ActionFunc) = copy(onActionFuncs = onActionFuncs :+ f)

    def onPosEdge(port: Port)(f: Sim => Unit) = onAction {
      case (sim, `port`, true, oldVal) if oldVal != Some(true) => f(sim)
      case _ => // do nothing
    }
    def onNegEdge(port: Port)(f: Sim => Unit) = onAction {
      case (sim, `port`, false, oldVal) if oldVal != Some(false) => f(sim)
      case _ => // do nothing
    }
    def whenHigh(port: Port)(f: Sim => Unit) = onAction {
      case (sim, _, _, _) if sim.isHigh(port) => f(sim)
      case _ => // do nothing
    }
    def whenLow(port: Port)(f: Sim => Unit) = onAction {
      case (sim, _, _, _) if sim.isLow(port) => f(sim)
      case _ => // do nothing
    }

    def check(f: Sim => Result) = copy(checkFunc = f)

    def run(): Prop = {
      given Arbitrary[Port] = Arbitrary(Gen.oneOf(ports.map(_._1)))

      testCases.foreach(runTestCase)
      forAll { (actions: List[(Port, Boolean)]) =>
        runTestCase(actions)
      }
    }

    def runTestCase(actions: Seq[(Port, Boolean)]): Result = {
      var sim = Sim.setupAndRun(comp)
      ports.foreach { case (port, newVal) => sim = sim.set(port, newVal) }
      sim = sim.run()
      onStartFunc(sim)

      foreach(actions) { case (port, newVal) =>
        val oldVal = sim.get(port)
        beforeActionFuncs.foreach { f => sim = f(sim, port, newVal, oldVal) }

        sim = sim.set(port, Some(newVal)).run()

        onActionFuncs.foreach(_(sim, port, newVal, oldVal))
        checkFunc(sim)
      }
    }
  }
}
