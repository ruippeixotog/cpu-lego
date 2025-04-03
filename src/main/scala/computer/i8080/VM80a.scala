package computer.i8080

import java.nio.file.{Files, Paths}

import component.BuilderAPI.buildComponent
import component.i8080.*
import core.CompositeComponent
import util.GraphvizDrawer

object VM80aApp extends App {
  val (_, comp) = buildComponent { vm80a(VM80Input()) }

  val dot = GraphvizDrawer.toDot(comp.asInstanceOf[CompositeComponent])
  Files.writeString(Paths.get("vm80a.dot"), dot)

  println("VM80a component created successfully.")
}
