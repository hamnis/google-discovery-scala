import scala.collection.immutable.Seq

val circeVersion = "0.14.6"

val scala212 = "2.12.18"
val scala213 = "2.13.13"
val scala3 = "3.3.1"

val baseVersion = "0.6"

inThisBuild(
  Seq(
    myBaseVersion := baseVersion,
    organization := "net.hamnaberg",
    sonatypeProfileName := organization.value,
    githubWorkflowJavaVersions := Seq(JavaSpec.temurin("17")),
    githubWorkflowTargetTags ++= Seq("v*"),
    githubWorkflowPublishTargetBranches :=
      Seq(RefPredicate.StartsWith(Ref.Tag("v")), RefPredicate.Equals(Ref.Branch("main"))),
    githubWorkflowPublish := Seq(
      WorkflowStep.Run(
        name = Some("Import signing key"),
        commands = List(
          """echo "$PGP_SECRET" | base64 -d -i - > /tmp/signing-key.gpg""",
          """echo "$PGP_PASSPHRASE" | gpg --pinentry-mode loopback --passphrase-fd 0 --import /tmp/signing-key.gpg"""
        ),
        env = Map(
          "PGP_PASSPHRASE" -> "${{ secrets.PGP_PASSPHRASE }}",
          "PGP_SECRET" -> "${{ secrets.PGP_SECRET }}"
        )
      ),
      WorkflowStep.Sbt(
        commands = List("+aetherDeploy", "sonatypeBundleReleaseIfRelevant"),
        name = Some("Publish project"),
        env = Map(
          "SONATYPE_PASSWORD" -> "${{ secrets.SONATYPE_PASSWORD }}",
          "SONATYPE_USERNAME" -> "${{ secrets.SONATYPE_USERNAME }}",
          "PGP_PASSPHRASE" -> "${{ secrets.PGP_PASSPHRASE }}"
        )
      )
    ),
    githubWorkflowBuild := Seq(
      WorkflowStep.Use(UseRef.Public("stringbean", "scalafmt-action", "v3")),
      WorkflowStep.Sbt(
        commands = List("+test", "sbtPlugin/scripted"),
        name = Some("Build project"),
        env = Map(
          "JAVA_TOOL_OPTIONS" -> "-Xss10M"
        )
      )
    ),
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

def sonatypeBundleReleaseIfRelevant: Command =
  Command.command("sonatypeBundleReleaseIfRelevant") { state =>
    if (state.getSetting(isSnapshot).getOrElse(false))
      state // a snapshot is good-to-go
    else // a non-snapshot releases as a bundle
      Command.process("sonatypeBundleRelease", state)
  }

def doConfigure(project: Project): Project =
  project
    .settings(
      publishTo := sonatypePublishToBundle.value,
      publishMavenStyle := true,
      commands += sonatypeBundleReleaseIfRelevant,
      sbtPluginPublishLegacyMavenStyle := false
    )
    .enablePlugins(MyVersioningPlugin)

val core = (projectMatrix in file("core"))
  .jvmPlatform(scalaVersions = Seq(scala212, scala213, scala3))
  .configure(doConfigure)
  .settings(
    name := "google-discovery-core",
    javacOptions ++= List("--release", "8"),
    scalacOptions ++= List("-deprecation", "-feature", "-language:higherKinds"),
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core" % circeVersion,
      "io.circe" %% "circe-generic" % circeVersion,
      "io.circe" %% "circe-jawn" % circeVersion,
      "org.http4s" %% "http4s-core" % "0.23.25",
      "org.http4s" %% "http4s-circe" % "0.23.25",
      "org.http4s" %% "http4s-client" % "0.23.25",
      "org.scalameta" %% "munit" % "1.0.0-M10" % Test,
      "org.typelevel" %% "munit-cats-effect" % "2.0.0-M4" % Test,
      "org.typelevel" %% "paiges-cats" % "0.4.3",
      "org.scala-lang.modules" %% "scala-collection-compat" % "2.11.0"
    ),
    Compile / doc / scalacOptions ++= Seq(
      "-no-link-warnings" // Suppresses problems with Scaladoc @throws links
    )
  )

val sbtPlugin = (projectMatrix in file("sbtPlugin"))
  .jvmPlatform(scalaVersions = Seq(scala212))
  .configure(doConfigure)
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

lazy val root = project
  .in(file("."))
  .configure(doConfigure)
  .aggregate((core.projectRefs ++ sbtPlugin.projectRefs): _*)
  .settings(
    name := "google-discovery",
    publish / skip := true,
    publishTo := None
  )
