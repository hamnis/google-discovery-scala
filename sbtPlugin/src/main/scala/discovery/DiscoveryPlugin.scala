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
  }
  import autoImport._

  override val projectSettings = Seq(
    Compile / sourceGenerators += Compile / discoveryGenerate,
    Compile / packageSrc / mappings ++= {
      val base = (Compile / sourceManaged).value
      val files = (Compile / managedSources).value
      files.map(f => (f, f.relativeTo(base).get.getPath))
    },
    Compile / discoveryDocument := parseDiscovery(baseDirectory.value),
    Compile / discoveryGenerate := {
      val discovery = (Compile / discoveryDocument).value
      val mangedDir = (Compile / sourceManaged).value / "scala"
      val packageName = discoveryPackage.value

      Codegen.generateFromDiscovery(packageName, discovery).map(_.writeTo(mangedDir.toPath).toFile)
    }
  )

  def discoveryDocumentFile(basedir: File) =
    basedir / "src" / "main" / "discovery" / "discovery.json"

  def parseDiscovery(basedir: File) = {
    val f = discoveryDocumentFile(basedir)
    jawn.decodeFile[Discovery](f).fold(throw _, identity)
  }
}
