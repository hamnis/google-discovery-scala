val circeVersion = "0.14.17"

val scala3 = "3.8.4"

val baseVersion = "0.7"

val http4sVersion = "0.23.38"

inThisBuild(
  Seq(
    myBaseVersion := baseVersion,
    organization := "net.hamnaberg",
    sonaDeploymentName := organization.value,
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
        commands = List("publishSigned", "sonaRelease"),
        name = Some("Publish project"),
        env = Map(
          "SONATYPE_PASSWORD" -> "${{ secrets.SONATYPE_PASSWORD }}",
          "SONATYPE_USERNAME" -> "${{ secrets.SONATYPE_USERNAME }}",
          "PGP_PASSPHRASE" -> "${{ secrets.PGP_PASSPHRASE }}"
        )
      )
    ),
    githubWorkflowBuild := Seq(
      WorkflowStep.Use(
        UseRef.Public("jrouly", "scalafmt-native-action", "v5"),
        params = Map("arguments" -> "--test")),
      WorkflowStep.Sbt(
        commands = List("+test", "sbtPlugin/scripted"),
        name = Some("Build project"),
        env = Map(
          "JAVA_TOOL_OPTIONS" -> "-Xss10M"
        )
      )
    ),
    homepage := Some(uri("https://github.com/hamnis/google-discovery-scala")),
    licenses := List(License.Apache2),
    developers := List(
      Developer(
        "hamnis",
        "Erlend Hamnaberg",
        "erlend@hamnaberg.net",
        uri("https://github.com/hamnis")
      )
    ),
    publishTo := {
      val centralSnapshots = "https://central.sonatype.com/repository/maven-snapshots/"
      if (isSnapshot.value) Some("central-snapshots".at(centralSnapshots))
      else localStaging.value
    }
  ))

def doConfigure(project: Project): Project =
  project
    .enablePlugins(MyVersioningPlugin)

val core = (projectMatrix in file("core"))
  .jvmPlatform(scalaVersions = Seq(scala3))
  .configure(doConfigure)
  .settings(
    name := "google-discovery-core",
    javacOptions ++= List("--release", "17"),
    scalacOptions ++= List("-deprecation", "-feature", "-language:higherKinds"),
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core" % circeVersion,
      "io.circe" %% "circe-generic" % circeVersion,
      "io.circe" %% "circe-jawn" % circeVersion,
      "org.http4s" %% "http4s-core" % http4sVersion,
      "org.http4s" %% "http4s-circe" % http4sVersion,
      "org.http4s" %% "http4s-client" % http4sVersion,
      "org.scalameta" %% "munit" % "1.3.6" % Test,
      "org.typelevel" %% "munit-cats-effect" % "2.2.1" % Test,
      "org.typelevel" %% "paiges-cats" % "0.4.4"
    ),
    Compile / doc / scalacOptions ++= Seq(
      "-no-link-warnings" // Suppresses problems with Scaladoc @throws links
    )
  )

val sbtPlugin = (projectMatrix in file("sbtPlugin"))
  .jvmPlatform(scalaVersions = Seq(scala3))
  .configure(doConfigure)
  .enablePlugins(SbtPlugin)
  .dependsOn(core)
  .settings(
    scalaVersion := scala3,
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
  .aggregate((core.projectRefs ++ sbtPlugin.projectRefs) *)
  .settings(
    name := "google-discovery",
    publish / skip := true,
    publishTo := None
  )
