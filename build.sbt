name := "computer-simulator"
organization := "net.ruippeixotog"

scalaVersion := "3.1.3"

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.16.0" % "test",
  "org.specs2" %% "specs2-core" % "5.0.1" % "test",
  "org.specs2" %% "specs2-scalacheck" % "5.0.1" % "test"
)

scalacOptions ++= Seq(
  "-feature",
  "-language:implicitConversions",
  "-no-indent",
  "-unchecked"
)

Test / fork := true
Test / parallelExecution := false
