package computer.i8080

import java.nio.file.{Files, Paths}

import component.BuilderAPI.buildComponent
import component.i8080.*
import core.*
import util.GraphvizDrawer

object VM80aApp extends App {
  val (_, comp) = buildComponent {
    val in = VM80Input()
    vm80a(in)
  }

  val dot = GraphvizDrawer.toDot(comp.asInstanceOf[CompositeComponent])
  Files.writeString(Paths.get("vm80a.dot"), dot)

  println("VM80a component created successfully.")
}
