enablePlugins(SbtPlugin)

organization := "net.hamnaberg.sbt"
name := "sbt-google-discovery"

val circeVersion = "0.14.6"
libraryDependencies ++= Seq(
  "io.circe" %% "circe-core" % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-parser" % circeVersion,
  "org.scalameta" %% "munit" % "0.7.29" % Test
)

ThisBuild / scriptedBufferLog := false

ThisBuild / scriptedLaunchOpts := {
  scriptedLaunchOpts.value ++
    Seq("-Xmx1024M", "-Dplugin.version=" + (ThisBuild / version).value)
}
