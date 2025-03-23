name := "cpu-lego"
organization := "net.ruippeixotog"

scalaVersion := "3.6.4"

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.18.1" % "test",
  "org.specs2" %% "specs2-core" % "5.5.8" % "test",
  "org.specs2" %% "specs2-scalacheck" % "5.5.8" % "test"
)

scalacOptions ++= Seq(
  "-feature",
  "-language:implicitConversions",
  "-no-indent",
  "-unchecked"
)

Test / fork := true
Test / parallelExecution := false
