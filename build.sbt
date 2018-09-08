ThisBuild / organization := "jp.sndyuk"
ThisBuild / scalaVersion := "2.12.6"
ThisBuild / version      := "0.1.0"

lazy val commonSettings = Seq(
  scalaVersion := "2.12.6",
  EclipseKeys.withSource := true,
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
      "ch.qos.logback" % "logback-classic" % "1.2.3",
      "com.typesafe.scala-logging" %% "scala-logging" % "3.9.0",
    ),
    publishTo := {
      val nexus = "https://oss.sonatype.org/"
      if (isSnapshot.value)
        Some("snapshots" at nexus + "content/repositories/snapshots") 
      else
        Some("releases"  at nexus + "service/local/staging/deploy/maven2")
    },
    credentials += Credentials(Path.userHome / ".sbt" / ".credentials"),
  )
  .dependsOn(template)

lazy val sample = (project in file("modules/sample"))
  .settings(
    commonSettings,
    name := "bunsan-shogi-sample",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.0.5" % "test",
      "org.scala-lang.modules" %% "scala-swing" % "2.0.3",
    )
  )
  .dependsOn(core)


pomExtra in ThisBuild :=
  <url>https://github.com/sndyuk/bunsan-shogi</url>
  <licenses>
    <license>
      <name>Apache License, Version 2.0</name>
      <url>https://github.com/sndyuk/bunsan-shogi/blob/master/LICENSE</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <url>https://github.com/sndyuk/bunsan-shogi.git</url>
    <connection>https://github.com/sndyuk/bunsan-shogi.git</connection>
  </scm>
  <developers>
    <developer>
      <id>sndyuk</id>
      <name>sndyuk</name>
      <url>https://github.com/sndyuk</url>
    </developer>
  </developers>
