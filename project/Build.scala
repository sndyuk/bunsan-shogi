import sbt._
import Keys._
import scala.Console
import com.typesafe.sbteclipse.plugin.EclipsePlugin._

object ApplicationBuild extends Build {

  println(Console.RED + """
    |    _______   ______  ___    ____  ______
    |   / ____/ | / / __ \/   |  / __ \/_  __/
    |  / __/ /  |/ / /_/ / /| | / /_/ / / /   
    | / /___/ /|  / _, _/ ___ |/ ____/ / /    
    |/_____/_/ |_/_/ |_/_/  |_/_/     /_/     
    """.stripMargin + Console.RESET)

  val appScalaVersion = "2.11.1"

  // --- Setting keys
  val appResolvers = Seq(
     "Sonatype OSS Releases"  at "http://oss.sonatype.org/content/repositories/releases",
     "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
     "Java.net"               at "http://download.java.net/maven/2")

  val appScalaOptions = Seq(
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
 
  val akkaVersion = "2.3.6"
  val orientdbVersion = "2.0-M3"

  val appDependencies = Seq(
      "org.scalatest" %% "scalatest" % "2.2.1" % "test",
      "org.slf4j" % "slf4j-api" % "1.7.2",
      "ch.qos.logback" % "logback-classic" % "1.0.7",
      "com.twitter" %% "util-collection" % "6.22.1",
      "com.orientechnologies" % "orientdb-enterprise" % orientdbVersion,
      "com.orientechnologies" % "orientdb-object" % orientdbVersion,
      "com.orientechnologies" % "orientdb-server" % orientdbVersion,
      "com.orientechnologies" % "orientdb-client" % orientdbVersion,
      "com.typesafe" % "config" % "1.2.1",
      "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0",
      "com.typesafe.akka" %% "akka-actor" % akkaVersion,
      "com.typesafe.akka" %% "akka-kernel" % akkaVersion,
      "com.typesafe.akka" %% "akka-remote" % akkaVersion,
      "org.scala-lang" % "scala-compiler" % appScalaVersion,
      "org.scala-lang" % "scala-reflect" % appScalaVersion,
      "org.scalafx" %% "scalafx" % "8.0.20-R6")

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
