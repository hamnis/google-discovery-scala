ThisBuild / version := "0.1"

name := "bigquery"

scalaVersion := "2.13.11"

enablePlugins(DiscoveryPlugin)

discoveryPackage := "bigquery"

val circeVersion = "0.14.6"
libraryDependencies ++= Seq(
  "io.circe" %% "circe-core" % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "org.scodec" %% "scodec-bits" % "1.1.37"
)
