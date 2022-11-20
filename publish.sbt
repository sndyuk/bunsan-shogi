ThisBuild / organizationName := "sndyuk"
ThisBuild / organizationHomepage := Some(url("https://github.com/sndyuk"))

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/sndyuk/bunsan-shogi"),
    "scm:git@github.com:sndyuk/bunsan-shogi.git"
  )
)
ThisBuild / developers := List(
  Developer(
    id    = "sndyuk",
    name  = "sndyuk",
    email = "sanada@sndyuk.com",
    url   = url("https://github.com/sndyuk")
  )
)

ThisBuild / description := "Scala Shogi Engine"
ThisBuild / licenses := List("Apache 2" -> new URL("http://www.apache.org/licenses/LICENSE-2.0.txt"))
ThisBuild / homepage := Some(url("https://github.com/sndyuk/bunsan-shogi"))

// Remove all additional repository other than Maven Central from POM
ThisBuild / pomIncludeRepository := { _ => false }
ThisBuild / publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}
ThisBuild / publishMavenStyle := true

ThisBuild / credentials += Credentials(Path.userHome / ".sbt" / ".credentials")
