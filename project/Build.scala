import sbt._
import Keys._
import scala.Console
import com.typesafe.sbteclipse.plugin.EclipsePlugin._

object ApplicationBuild extends Build {

  val appScalaVersion = "2.11.12"

  // --- Setting keys
  val appResolvers = Seq(
     DefaultMavenRepository,
     Resolver.sonatypeRepo("public"),
     Resolver.typesafeRepo("releases"),
     JavaNet1Repository)

  val appScalaOptions = Seq(
      "-target:jvm-1.8",
      "-deprecation",
      "-unchecked",
      "-feature",
      "-optimize",
      "-Xlog-reflective-calls",
      "-Ywarn-adapted-args",
      "-Yinline-warnings",
      "-language:postfixOps",
      //"-Xprint:jvm",
      //"-Ymacro-debug-lite",
      "-encoding", "utf-8")

      val akkaVersion = "2.4.20"
      val orientdbVersion = "2.1.25"

  val appDependencies = Seq(
      "org.scalatest" %% "scalatest" % "2.2.6" % "test",
      "com.typesafe.scala-logging" %% "scala-logging" % "3.9.0",
      "com.twitter" %% "util-core" % "18.5.0",
      "com.orientechnologies" % "orientdb-enterprise" % orientdbVersion,
      "com.orientechnologies" % "orientdb-object" % orientdbVersion,
      "com.orientechnologies" % "orientdb-server" % orientdbVersion,
      "com.orientechnologies" % "orientdb-client" % orientdbVersion,
      "com.typesafe" % "config" % "1.3.3",
      "org.scala-lang" % "scala-compiler" % appScalaVersion,
      "org.scala-lang" % "scala-reflect" % appScalaVersion,
      "org.scala-lang.modules" %% "scala-swing" % "2.0.2",
      "com.typesafe.akka" %% "akka-actor" % akkaVersion,
      "com.typesafe.akka" %% "akka-kernel" % akkaVersion,
      "com.typesafe.akka" %% "akka-remote" % akkaVersion)

  lazy val macro = Project(id = "bunsan-shogi-macro", base = file("modules/macro")).settings( 
    scalaVersion := appScalaVersion,
    scalacOptions ++= appScalaOptions,
    libraryDependencies ++= appDependencies,
    resolvers ++= appResolvers,
    EclipseKeys.withSource := true,
    EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Resource
  )



  lazy val main = Project(id = "bunsan-shogi", base = file("modules/main"),
    settings = Defaults.defaultSettings ++ Seq(
      unmanagedResourceDirectories in Compile <<= (sourceDirectory, unmanagedResourceDirectories in Compile) {
        (dir, res) => 
         res foreach(println)
         res
      }
    )
  ).settings(

    // --- Scala
    scalaVersion := appScalaVersion,
    scalacOptions ++= appScalaOptions,
    libraryDependencies ++= appDependencies,
    resolvers ++= appResolvers,
    EclipseKeys.withSource := true,
    EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Resource
  ).dependsOn(macro)
}

