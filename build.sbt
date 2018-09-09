ThisBuild / organization := "com.sndyuk"
ThisBuild / scalaVersion := "2.12.6"
ThisBuild / version      := "0.1.0"

lazy val commonSettings = Seq(
  scalaVersion := "2.12.6",
  EclipseKeys.withSource := true,
  scalacOptions ++= Seq(
    "-Xfatal-warnings",
    "-deprecation",
    "-Xlint",
    "-opt:unreachable-code,simplify-jumps,compact-locals,redundant-casts,box-unbox,l:inline",
    "-opt-inline-from:jp.sndyuk.shogi.**",
  )
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
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1",
      "com.typesafe" % "config" % "1.3.3",
    ),
  )
  .dependsOn(template)

lazy val sample = (project in file("modules/sample"))
  .settings(
    commonSettings,
    name := "bunsan-shogi-sample",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.0.5" % "test",
      "ch.qos.logback" % "logback-classic" % "1.2.3",
      "org.scala-lang.modules" %% "scala-swing" % "2.0.3",
    )
  )
  .dependsOn(core)
