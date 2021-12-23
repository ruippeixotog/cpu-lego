package testkit

import core._
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Prop}
import org.specs2.ScalaCheck
import org.specs2.execute.{AsResult, Result}
import org.specs2.mutable.Specification
import simulator.{PortChange, Sim, SimState}

trait SequentialScenarios { this: Specification with ScalaCheck =>

  case class SequentialScenario(
      comp: Component,
      ports: Seq[(Port, Option[Boolean])] = Nil,
      testCases: Seq[Seq[(Port, Boolean)]] = Nil,
      onStartFunc: SimState => Unit = _ => {},
      filterFunc: SimState => Boolean = _ => true,
      beforeActionFunc: (SimState, Port, Boolean, Option[Boolean]) => Unit = (_, _, _, _) => {},
      onActionFunc: (SimState, Port, Boolean, Option[Boolean]) => Unit = (_, _, _, _) => {},
      checkFunc: SimState => Result = _ => ok
  ) {
    def withPorts(ports: (Port, Option[Boolean])*) = copy(ports = ports)
    def withTestCases(testCases: Seq[(Port, Boolean)]*) = copy(testCases = testCases)
    def onStart(f: SimState => Unit) = copy(onStartFunc = f)
    def beforeAction(f: (SimState, Port, Boolean, Option[Boolean]) => Unit) = copy(beforeActionFunc = f)
    def onAction(f: (SimState, Port, Boolean, Option[Boolean]) => Unit) = copy(onActionFunc = f)
    def check(f: SimState => Result) = copy(checkFunc = f)

    def run(): Prop = {
      given Arbitrary[Port] = Arbitrary(Gen.oneOf(ports.map(_._1)))

      testCases.foreach(runTestCase)
      forAll { (actions: List[(Port, Boolean)]) =>
        runTestCase(actions)
      }
    }

    def runTestCase(actions: Seq[(Port, Boolean)]): Result = {
      var state = Sim.runComponent(comp)
      ports.foreach { case (port, newVal) => state.schedule(0, PortChange(port, newVal)) }
      state = Sim.run(state)
      onStartFunc(state)

      foreach(actions) { case (port, newVal) =>
        val oldVal = state.get(port)
        beforeActionFunc(state, port, newVal, oldVal)

        state.schedule(0, PortChange(port, Some(newVal)))
        state = Sim.run(state)

        onActionFunc(state, port, newVal, oldVal)
        checkFunc(state)
      }
    }
  }
}
