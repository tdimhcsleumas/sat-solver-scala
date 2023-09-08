import LibraryDependencies._

ThisBuild / scalaVersion := "2.12.3"
ThisBuild / organization := "tdimhcsleumas"
ThisBuild / autoCompilerPlugins := true

javaOptions += "-Dorg.slf4j.simpleLogger.defaultLogLevel=debug"

lazy val root = (project in file("."))
    .aggregate(`sat-solver-cli`)
    .aggregate(`sat-solver-lib`)
    .aggregate(`sudoku-lib`)

lazy val `sat-solver-cli` = (project in file("sat-solver-cli"))
    .dependsOn(`sat-solver-lib`)
    .settings(commonSettings)

lazy val `sat-solver-lib` = (project in file("sat-solver-lib"))
    .settings(commonSettings)

lazy val `sudoku-lib` = (project in file("sudoku-lib"))
    .dependsOn(`sat-solver-lib`)
    .settings(commonSettings)

lazy val compilerOptions = Seq(
  "-P:bm4:no-filtering:y",
  "-Ypartial-unification",
  "-Ywarn-unused",
  "-unchecked",
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:postfixOps",
  "-deprecation",
  "-encoding",
  "utf8"
)
 
lazy val commonDependencies = Seq(
    ScalaTest.scalaTest % Test,
    Circe.circeCore,
    Circe.circeGeneric,
    Circe.circeParser,
    Log4s.log4s,
    SLF4j.logger,
)

lazy val compilerPlugins = Seq(
  compilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
)

lazy val commonSettings = Seq(
  scalacOptions ++= compilerOptions,
  Test / publishArtifact := false,
  resolvers ++= Seq(
    "Local Maven Repository".at(
      "file://" + Path.userHome.absolutePath + "/.m2/repository"
    ),
    "Local Ivy2 Repository".at(
      "file://" + Path.userHome.absolutePath + "/.ivy2/local"
    ),
    "Typesafe repository".at("https://dl.bintray.com/typesafe/maven-releases/")
  ),
  libraryDependencies ++= commonDependencies ++ compilerPlugins,
  evictionErrorLevel := Level.Info,
  Test / parallelExecution := false,
  Test / fork := false,
)
