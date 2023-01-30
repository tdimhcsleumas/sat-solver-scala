ThisBuild / scalaVersion := "2.13.6"
ThisBuild / organization := "com.github.tdimhcsleumas"

lazy val satSolver = (project in file("."))
    .settings(
        name :=  "satSolver"
    )