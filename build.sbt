inThisBuild(Seq(
  organization := "net.hamnaberg",
  githubWorkflowJavaVersions := Seq(JavaSpec.temurin("17")),
  githubWorkflowTargetTags ++= Seq("v*"),
  githubWorkflowPublishTargetBranches :=
   Seq(RefPredicate.StartsWith(Ref.Tag("v"))),
  githubWorkflowPublish := Seq(
    WorkflowStep.Sbt(
      commands = List("ci-release"),
      name = Some("Publish project"),
      env = Map(
        "PGP_PASSPHRASE" -> "${{ secrets.PGP_PASSPHRASE }}",
        "PGP_SECRET" -> "${{ secrets.PGP_SECRET }}",
        "SONATYPE_PASSWORD" -> "${{ secrets.SONATYPE_PASSWORD }}",
        "SONATYPE_USERNAME" -> "${{ secrets.SONATYPE_USERNAME }}"
      )
    )
  ),
  crossScalaVersions := Seq(scala212, scala213, scala3),
  scalaVersion := crossScalaVersions.value.head,
  githubWorkflowBuild := Seq(WorkflowStep.Sbt(
    commands = List("+test", "sbtPlugin/scripted"),
    name = Some("Build project"),
    env = Map(
      "JAVA_TOOL_OPTIONS" -> "-Xss10M"
    )
  )),
  githubWorkflowBuildSbtStepPreamble := Nil,
  githubWorkflowScalaVersions := List("all"),
  githubWorkflowGeneratedDownloadSteps := Nil,
  githubWorkflowArtifactUpload := false,
  homepage := Some(url("https://github.com/hamnis/google-discovery-scala")),
  licenses := List(License.Apache2),
  developers := List(
    Developer(
      "hamnis",
      "Erlend Hamnaberg",
      "erlend@hamnaberg.net",
      url("https://github.com/hamnis")
    )
  )
))

val circeVersion = "0.14.6"

val scala212 = "2.12.18"
val scala213 = "2.13.12"
val scala3 = "3.3.1"

val core = project
  .in(file("core"))
  .settings(
    name := "google-discovery-core",
    javacOptions ++= List("--release", "8"),
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

lazy val root = project.in(file(".")).settings(
  name := "google-discovery",

  publish := {},
  publishLocal := {},
  publishArtifact := false,
  publish / skip := true

)