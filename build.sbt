val circeVersion = "0.14.6"

val scala212 = "2.12.18"

val baseVersion = "0.1"

inThisBuild(Seq(
  organization := "net.hamnaberg",
  sonatypeProfileName := organization.value,
  githubWorkflowJavaVersions := Seq(JavaSpec.temurin("17")),
  githubWorkflowTargetTags ++= Seq("v*"),
  githubWorkflowPublishTargetBranches :=
   Seq(RefPredicate.StartsWith(Ref.Tag("v"))),
  githubWorkflowPublish := Seq(
    WorkflowStep.Run(
      name = Some("Import signing key"),
      commands = List(
        """echo "$PGP_SECRET" | base64 -d -i - > /tmp/signing-key.gpg""",
        """echo "$PGP_PASSPHRASE" | gpg --pinentry-mode loopback --passphrase-fd 0 --import /tmp/signing-key.gpg""",
        //"""(echo "$PGP_PASSPHRASE"; echo; echo) | gpg --command-fd 0 --pinentry-mode loopback --change-passphrase $(gpg --list-secret-keys --with-colons 2> /dev/null | grep '^sec:' | cut --delimiter ':' --fields 5 | tail -n 1)"""
      ),
      env = Map(
        "PGP_PASSPHRASE" -> "${{ secrets.PGP_PASSPHRASE }}",
        "PGP_SECRET" -> "${{ secrets.PGP_SECRET }}",
      )
    ),
    WorkflowStep.Sbt(
      commands = List("+publishSigned", "sonatypeBundleRelease"),
      name = Some("Publish project"),
      env = Map(
        "SONATYPE_PASSWORD" -> "${{ secrets.SONATYPE_PASSWORD }}",
        "SONATYPE_USERNAME" -> "${{ secrets.SONATYPE_USERNAME }}",
        "PGP_PASSPHRASE" -> "${{ secrets.PGP_PASSPHRASE }}",
      )
    )
  ),
  //crossScalaVersions := Seq(scala212, scala213, scala3),
  scalaVersion := scala212,
  githubWorkflowBuild := Seq(WorkflowStep.Sbt(
    commands = List("+test", "sbtPlugin/scripted"),
    name = Some("Build project"),
    env = Map(
      "JAVA_TOOL_OPTIONS" -> "-Xss10M"
    )
  )),
  /*githubWorkflowBuildSbtStepPreamble := Nil,
  githubWorkflowScalaVersions := List("all"),
  githubWorkflowGeneratedDownloadSteps := Nil,
  githubWorkflowArtifactUpload := false,*/
  homepage := Some(url("https://github.com/hamnis/google-discovery-scala")),
  licenses := List(License.Apache2),
  developers := List(
    Developer(
      "hamnis",
      "Erlend Hamnaberg",
      "erlend@hamnaberg.net",
      url("https://github.com/hamnis")
    )
  ),
  git.baseVersion := baseVersion,
  git.formattedShaVersion := None,
  git.formattedDateVersion := git.baseVersion.value + "-SNAPSHOT"
))

def doConfigure(project: Project): Project = {
  project.settings(
    publishTo := sonatypePublishToBundle.value,
    publishMavenStyle := true,
  ).enablePlugins(GitVersioning)
}

val core = project
  .in(file("core"))
  .configure(doConfigure)
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

lazy val root = project.in(file(".")).aggregate(core, sbtPlugin).settings(
  name := "google-discovery",
  publish / skip := true,
  publishTo := None
)