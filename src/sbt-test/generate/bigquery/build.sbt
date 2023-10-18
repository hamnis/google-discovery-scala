ThisBuild / version := "0.1"

name := "bigquery"

crossScalaVersions := List("2.13.11", "2.12.18", "3.3.1")
scalaVersion := crossScalaVersions.value.head

scalacOptions ++= {
  if (scalaBinaryVersion.value == "3") Seq("-Xmax-inlines", "128", "-J-Xss3m") else Nil
}

enablePlugins(DiscoveryPlugin)

discoveryPackage := "bigquery"

val circeVersion = "0.14.6"
libraryDependencies ++= Seq(
  "io.circe" %% "circe-core" % circeVersion,
  "org.scodec" %% "scodec-bits" % "1.1.37"
)
