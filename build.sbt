ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.1"

lazy val root = (project in file("."))
  .settings(
    name := "aoc",
    idePackagePrefix := Some("se.jakub")
  )

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.17" % "test"

libraryDependencies +=
  "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"

libraryDependencies += "org.jgrapht" % "jgrapht-core" % "1.5.1" // use the latest version
