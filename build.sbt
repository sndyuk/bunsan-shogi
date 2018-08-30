ThisBuild / organization := "jp.sndyuk"
ThisBuild / scalaVersion := "2.12.6"
ThisBuild / version      := "0.1.0-SNAPSHOT"

lazy val orientdbVersion = "3.0.6"

lazy val commonSettings = Seq(
  scalaVersion := "2.12.6"
)

lazy val root = (project in file("."))
  .aggregate(
    template,
    core
  )

lazy val template = (project in file("modules/template"))
  .settings(
    commonSettings,
    name := "bunsan-shogi-template"
  )

lazy val core = (project in file("modules/core"))
  .settings(
    commonSettings,
    name := "bunsan-shogi-core",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.0.5" % "test",
      "com.typesafe.scala-logging" %% "scala-logging" % "3.9.0",
      "com.twitter" %% "util-core" % "18.5.0",
      "com.typesafe" % "config" % "1.3.3",
      "org.scala-lang.modules" %% "scala-swing" % "2.0.3",
      "ch.qos.logback" % "logback-classic" % "1.2.3",
      "com.typesafe.scala-logging" %% "scala-logging" % "3.9.0"
    )
  )
  .dependsOn(template)
