package discovery

import io.circe._
import sbt.Keys._
import sbt._

object DiscoveryPlugin extends AutoPlugin {

  override def trigger = noTrigger

  object autoImport extends scala.AnyRef {
    lazy val discoveryGenerate =
      taskKey[Seq[File]]("Generate Scala case classes for the given Discovery Document")
    lazy val discoveryPackage = settingKey[String]("Package for generated sources")
    lazy val discoveryDocument = taskKey[Discovery]("discovery document")
    lazy val discoveryFile = settingKey[File]("discovery document file")
  }
  import autoImport._

  override val projectSettings = Seq(
    Compile / sourceGenerators += Compile / discoveryGenerate,
    Compile / packageSrc / mappings ++= {
      val base = (Compile / sourceManaged).value
      val files = (Compile / managedSources).value
      files.map(f => (f, f.relativeTo(base).get.getPath))
    },
    Compile / discoveryFile := discoveryDocumentFile(baseDirectory.value),
    Compile / discoveryDocument := parseDiscovery((Compile / discoveryFile).value),
    Compile / discoveryGenerate := {
      val discovery = (Compile / discoveryDocument).value
      val mangedDir = (Compile / sourceManaged).value / "scala"
      val packageName = discoveryPackage.value

      Codegen.generateFromDiscovery(packageName, discovery).map(_.writeTo(mangedDir.toPath).toFile)
    }
  )

  def discoveryDocumentFile(basedir: File) = {
    val names = Set("jvm", "js")
    val resolved =
      if (names.contains(basedir.getName)) basedir.getParentFile / "shared" else basedir
    resolved / "src" / "main" / "discovery" / "discovery.json"
  }

  def parseDiscovery(file: File) =
    jawn.decodeFile[Discovery](file).fold(throw _, identity)
}
