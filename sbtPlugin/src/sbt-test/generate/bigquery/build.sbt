ThisBuild / version := "0.1"

name := "bigquery"

crossScalaVersions := List("2.13.12", "2.12.18", "3.3.1")
scalaVersion := crossScalaVersions.value.head

scalacOptions ++= Seq("-J-Xss5m")

enablePlugins(DiscoveryPlugin)

discoveryPackage := "bigquery"

val circeVersion = "0.14.6"
val http4sVersion = "0.23.23"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core" % circeVersion,
  "org.http4s" %% "http4s-circe" % http4sVersion,
  "org.http4s" %% "http4s-client" % http4sVersion,
  "org.scodec" %% "scodec-bits" % "1.1.37"
)
