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
    core,
    sample, // Re-enabled in aggregate
    web
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
      "org.scala-lang.modules" %% "scala-parser-combinators" % "2.0.0", // Updated version
      "com.typesafe" % "config" % "1.4.3",
      "com.typesafe.play" %% "play-json" % "2.9.4"
    ),
  )
  .dependsOn(template)

// Re-enabled the sample module definition
lazy val sample = (project in file("src/sample"))
  .settings(
    commonSettings,
    name := "bunsan-shogi-sample",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.18" % "test",
      "ch.qos.logback" % "logback-classic" % "1.2.13",
      "org.scala-lang.modules" %% "scala-swing" % "2.1.1",
    ),
    // Exclude Gui.scala from compilation as per README suggestion that it's disabled
    // excludeFilter in Compile := HiddenFileFilter || GlobFilter("Gui.scala")
  )
  .dependsOn(core)

lazy val web = (project in file("src/web"))
  .settings(
    commonSettings,
    name := "bunsan-shogi-web",
    libraryDependencies ++= Seq(
      "org.scalatra" %% "scalatra" % "2.8.4",
      "org.scalatra" %% "scalatra-scalatest" % "2.8.4" % "test",
      "ch.qos.logback" % "logback-classic" % "1.2.13",
      "org.eclipse.jetty" % "jetty-webapp" % "9.4.53.v20231009",
      "org.eclipse.jetty" % "jetty-server" % "9.4.53.v20231009",
      "org.eclipse.jetty" % "jetty-servlet" % "9.4.53.v20231009",
      "javax.servlet" % "javax.servlet-api" % "3.1.0" % "provided"
    )
  )
  .dependsOn(core)
