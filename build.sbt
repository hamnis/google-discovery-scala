ThisBuild / organization := "net.hamnaberg"
name := "google-discovery"

Compile / publishArtifact := false
Compile / doc / publishArtifact := false

val circeVersion = "0.14.6"

val scala212 = "2.12.18"
val scala213 = "2.13.12"
val scala3 = "3.3.1"

val core = project
  .in(file("core"))
  .settings(
    name := "google-discovery-core",
    crossScalaVersions := Seq(scala212, scala213, scala3),
    scalaVersion := crossScalaVersions.value.head,
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core" % circeVersion,
      "io.circe" %% "circe-generic" % circeVersion,
      "io.circe" %% "circe-parser" % circeVersion,
      "org.scalameta" %% "munit" % "0.7.29" % Test,
      "org.http4s" %% "http4s-core" % "0.23.23",
      "org.typelevel" %% "paiges-cats" % "0.4.3"
    )
  )

val sbtPlugin = project
  .in(file("sbtPlugin"))
  .enablePlugins(SbtPlugin)
  .dependsOn(core)
  .settings(
    scalaVersion := scala212,
    name := "google-discovery-sbt",
    ThisBuild / scriptedBufferLog := false,
    ThisBuild / scriptedLaunchOpts := {
      scriptedLaunchOpts.value ++
        Seq("-Xmx1024M", "-Dplugin.version=" + (ThisBuild / version).value)
    }
  )
