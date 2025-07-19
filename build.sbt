ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.3.6"

lazy val root = (project in file("."))
  .settings(
    name := "proyecto-funcional",
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4",
      "io.github.hughsimpson" %% "scalameter" % "0.22.1" % Test
    ),
    javaOptions ++= Seq(
      "-Xmx8G",
      "-XX:-UseGCOverheadLimit",
      "-Xms1G",
      "-XX:+UseG1GC"
    )
  )