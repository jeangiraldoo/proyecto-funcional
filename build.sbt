ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.3.6"

lazy val root = (project in file("."))
  .settings(
    name := "proyecto-funcional",
    libraryDependencies ++= Seq(
      ("com.storm-enroute" %% "scalameter-core" % "0.21").cross(CrossVersion.for3Use2_13),
      "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.3",
      "org.scalameta" %% "munit" % "0.7.26"%Test,
),
    javaOptions ++= Seq(
      "-Xmx8G",
      "-XX:-UseGCOverheadLimit",
      "-Xms1G",
      "-XX:+UseG1GC"
    )
  )