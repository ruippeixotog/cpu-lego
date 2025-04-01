package testkit

import java.nio.file.{Files, Paths}

import scala.io.Source

import org.specs2.execute.{Result, StandardResults}
import org.specs2.main.CommandLine
import org.specs2.matcher.{AnyMatchers, Matcher, MustMatchers}

trait SnapshotTests extends MustMatchers, AnyMatchers, StandardResults {
  def cmdArgs: CommandLine

  def matchSnapshot(snapshotRes: String): Matcher[String] =
    matchSnapshotImpl(snapshotRes, (actual, expected) => actual must beEqualTo(expected.mkString))

  def matchSnapshotLines(snapshotRes: String): Matcher[String] = matchSnapshotImpl(
    snapshotRes,
    (actual, expected) => actual.split("\n").toSeq must containTheSameElementsAs(expected.getLines.toSeq)
  )

  private def matchSnapshotImpl(snapshotRes: String, f: (String, Source) => Result): Matcher[String] =
    (actual: String) => {
      if (cmdArgs.contains("--update-snapshots")) {
        Files.writeString(Paths.get("src/test/resources", snapshotRes), actual)
        done
      } else {
        f(actual, Source.fromResource(snapshotRes))
      }
    }
}
