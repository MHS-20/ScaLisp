ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.3.3"

lazy val root = (project in file("."))
  .settings(
    name := "ScaLisp",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.17" % Test,
    run / fork := true,
    run / connectInput := true,
    run / outputStrategy := Some(StdoutOutput),
    scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")
  )
