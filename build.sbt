ThisBuild / scalaVersion := "2.13.6"
ThisBuild / organization := "tdimhcsleumas"

lazy val root = (project in file("."))
    .aggregate(`sat-solver-cli`)

lazy val `sat-solver-cli` = (project in file("sat-solver-cli"))
    .dependsOn(`sat-solver-lib`)

lazy val `sat-solver-lib` = (project in file("sat-solver-lib"))
    // .settings(commonSettings)
