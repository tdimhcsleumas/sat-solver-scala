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

    object Log4s {
        val version = "1.8.2"
        val log4s = "org.log4s" %% "log4s" % version
    }

    object SLF4j {
        val version = "2.0.7"
        val logger = "org.slf4j" % "slf4j-simple" % version
    }
}
