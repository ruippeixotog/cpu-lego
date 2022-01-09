name := "computer-simulator"
organization := "net.ruippeixotog"

scalaVersion := "3.1.0"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor-typed" % "2.6.18",
  "org.scalacheck" %% "scalacheck" % "1.15.4" % "test",
  "org.specs2" %% "specs2-core" % "5.0.0-RC-22" % "test",
  "org.specs2" %% "specs2-scalacheck" % "5.0.0-RC-22" % "test"
)

scalacOptions ++= Seq(
  "-feature",
  "-language:implicitConversions",
  "-no-indent",
  "-unchecked"
)

Test / fork := true
Test / parallelExecution := false
