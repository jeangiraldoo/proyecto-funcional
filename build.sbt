ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.6"

lazy val root = (project in file("."))
  .settings(
    name := "proyecto-funcional",
    libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"

)
