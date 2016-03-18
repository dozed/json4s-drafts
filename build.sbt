name := "json4s-drafts"

version := "1.0"

scalaVersion := "2.11.7"

crossScalaVersions := Seq("2.10.4", "2.11.7")

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

libraryDependencies ++= Seq(
  "org.json4s" %% "json4s-scalaz" % "3.3.0",
  "org.json4s" %% "json4s-jackson" % "3.3.0",
  "joda-time" % "joda-time" % "2.9.2",
  "com.chuusai" %% "shapeless" % "2.2.5",
  "org.scalaz" %% "scalaz-core" % "7.1.7",
  "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided",
  "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided",
  "org.typelevel" %% "macro-compat" % "1.1.1",
  compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
  compilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1")
)


// throws a java.lang.IllegalArgumentException: URI scheme is not "file"
// lazy val json4sScalaz = ProjectRef(uri("git://github.com/json4s/json4s.git"), "json4s-scalaz")
// lazy val json4sJackson = ProjectRef(uri("git://github.com/json4s/json4s.git"), "json4s-jackson")

lazy val root = project.in(file("."))
