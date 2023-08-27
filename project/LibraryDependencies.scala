import sbt._

object LibraryDependencies {
    object ScalaTest {
        val version = "3.2.16"
        val scalaTest = "org.scalatest" %% "scalatest" % "3.2.16"
    }

    object Circe {
        val version = "0.14.1"
        val circeCore = "io.circe" %% "circe-core" % version
        val circeGeneric = "io.circe" %% "circe-generic" % version
        val circeParser = "io.circe" %% "circe-parser" % version
    }
}
