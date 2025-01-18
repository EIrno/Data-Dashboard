ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.0"

lazy val root = (project in file("."))
  .settings(
    name := "investing_dashboard"
  )
libraryDependencies += "org.scalafx" % "scalafx_3" % "20.0.0-R31"
libraryDependencies += "com.softwaremill.sttp.client4" %% "core" % "4.0.0-M1"
libraryDependencies += "org.scala-lang" %% "toolkit" % "0.1.7"
libraryDependencies += "org.playframework" %% "play-json" % "3.0.2"


