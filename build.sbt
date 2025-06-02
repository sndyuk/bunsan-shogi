ThisBuild / organization := "com.sndyuk"
ThisBuild / scalaVersion := "2.12.18"
ThisBuild / version      := "0.1.2"

lazy val commonSettings = Seq(
  scalaVersion := "2.12.18",
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

lazy val template = (project in file("src/template"))
  .settings(
    commonSettings,
    name := "bunsan-shogi-template"
  )

lazy val core = (project in file("src/core"))
  .settings(
    commonSettings,
    name := "bunsan-shogi-core",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.18" % "test",
      "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5",
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
      "com.typesafe" % "config" % "1.4.3",
      "com.typesafe.play" %% "play-json" % "2.9.4" // Added play-json dependency
    ),
  )
  .dependsOn(template)

lazy val sample = (project in file("src/sample"))
  .settings(
    commonSettings,
    name := "bunsan-shogi-sample",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.18" % "test",
      "ch.qos.logback" % "logback-classic" % "1.2.13",
      "org.scala-lang.modules" %% "scala-swing" % "2.1.1",
    )
  )
  .dependsOn(core)
