name := "json4s-drafts"

version := "1.0"

scalaVersion := "2.11.7"

crossScalaVersions := Seq("2.10.4", "2.11.7")

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

libraryDependencies ++= Seq(
  "org.json4s" %% "json4s-jackson" % "3.3.0",
  "com.chuusai" %% "shapeless" % "2.3.1",
  "org.scalaz" %% "scalaz-core" % "7.2.2",
  "org.scalaz" %% "scalaz-concurrent" % "7.2.2" % "test",
  "joda-time" % "joda-time" % "2.9.2" % "test",
  "org.scalatra.rl" %% "rl" % "0.4.10" % "test",
  "net.databinder.dispatch" %% "dispatch-core" % "0.11.2" % "test",
  "org.specs2" %% "specs2-core" % "3.7.2" % "test",
  "org.scalacheck" %% "scalacheck" % "1.12.5" % "test",
  "org.typelevel" %% "scalaz-specs2" % "0.4.0" % "test",
  // "org.http4s" %% "http4s-blaze-client" % "0.12.1" % "test",
  compilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1")
)

exportJars := true

// throws a java.lang.IllegalArgumentException: URI scheme is not "file"
// lazy val json4sScalaz = ProjectRef(uri("git://github.com/json4s/json4s.git"), "json4s-scalaz")
// lazy val json4sJackson = ProjectRef(uri("git://github.com/json4s/json4s.git"), "json4s-jackson")

lazy val root = project.in(file(".")).copy(id = "json4s-drafts")
