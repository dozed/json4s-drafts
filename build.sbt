name := "json4s-drafts"

version := "1.0"

scalaVersion := "2.11.7"

crossScalaVersions := Seq("2.10.4", "2.11.7")

val paradiseVersion = "2.1.0"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

libraryDependencies ++= Seq(
  "org.json4s" %% "json4s-jackson" % "3.3.0",
  "com.chuusai" %% "shapeless" % "2.2.5",
  "org.scalaz" %% "scalaz-core" % "7.1.7",
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "org.typelevel" %% "macro-compat" % "1.1.1",
  compilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full),
  "joda-time" % "joda-time" % "2.9.2",
  compilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1")
)

libraryDependencies ++= {
  if (scalaVersion.value.startsWith("2.10"))
    List("org.scalamacros" %% "quasiquotes" % paradiseVersion)
  else Nil
}


