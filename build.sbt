ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.6.1"

lazy val root = (project in file("."))
  .settings(
    name := "JBInternshipTask",
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.17.1" % Test
  )
