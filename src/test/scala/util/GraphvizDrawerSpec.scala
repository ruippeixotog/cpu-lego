package util

import java.io.PrintStream
import java.nio.file.{Files, Path, Paths}

import component.BuilderAPI.*
import core.*
import org.specs2.main.CommandLine
import testkit.{BaseSpec, SnapshotTests}
import yosys.ComponentCreator

class GraphvizDrawerSpec(val cmdArgs: CommandLine) extends BaseSpec with SnapshotTests {
  val aoiCreator = ComponentCreator(Path.of(ClassLoader.getSystemResource("aoi.json").toURI()))
  val latchCreator = ComponentCreator(Path.of(ClassLoader.getSystemResource("dlatch.json").toURI()))

  def aoi(a: Port, b: Port, c: Port, d: Port): Spec[Port] = {
    val outs = aoiCreator.create("aoi", Map("A" -> a, "B" -> b, "C" -> c, "D" -> d))
    outs("F").asInstanceOf[Port]
  }

  def dLatch(d: Port, en: Port, rstn: Port): Spec[Port] = {
    val outs = latchCreator.create("d_latch", Map("d" -> d, "en" -> en, "rstn" -> rstn))
    outs("q").asInstanceOf[Port]
  }

  "GraphvizDrawer" should {

    "print AOI correctly" in {
      val a, b, c, d = new Port()
      val (f, comp) = buildComponent(Some("test_aoi"), aoi(a, b, c, d))

      val dot = GraphvizDrawer.toDot(comp.asInstanceOf[CompositeComponent])
      dot must matchSnapshotLines("aoi.expected.dot")
    }

    "print dLatch correctly" in {
      val d, en, rstn = new Port()
      val (f, comp) = buildComponent(Some("test_dlatch"), dLatch(d, en, rstn))

      val dot = GraphvizDrawer.toDot(comp.asInstanceOf[CompositeComponent])
      dot must matchSnapshotLines("dlatch.expected.dot")
    }
  }
}
