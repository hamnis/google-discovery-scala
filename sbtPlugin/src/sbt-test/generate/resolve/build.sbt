ThisBuild / version := "0.1"

name := "bigquery-resolve"

crossScalaVersions := List("2.13.12")
scalaVersion := crossScalaVersions.value.head

//scalacOptions ++= Seq("-J-Xss10M")

enablePlugins(ResolveDiscoveryPlugin)
enablePlugins(DiscoveryPlugin)

//discoveryUri := url("https://bigquery.googleapis.com/discovery/v1/apis/bigquery/v2/rest")
discoveryUri := url("http://localhost:25609/discovery.json")
discoveryPackage := "bigquery"

val circeVersion = "0.14.6"
val http4sVersion = "0.23.24"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core" % circeVersion,
  "org.http4s" %% "http4s-circe" % http4sVersion,
  "org.http4s" %% "http4s-client" % http4sVersion,
  "org.scodec" %% "scodec-bits" % "1.1.37"
)
