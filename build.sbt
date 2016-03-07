name := "json4s-drafts"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies += "org.json4s" %% "json4s-scalaz" % "3.3.0"
libraryDependencies += "org.json4s" %% "json4s-jackson" % "3.3.0"
libraryDependencies += "com.chuusai" %% "shapeless" % "2.2.5"
libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.7"

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1")
