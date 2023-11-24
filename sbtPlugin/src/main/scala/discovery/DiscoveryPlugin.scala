package discovery

import io.circe.*
import sbt.Keys.*
import sbt.*
import sbt.nio.Keys.*

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
    Compile / discoveryDocument := {
      val f = (Compile / discoveryGenerate).inputFiles.head
      jawn.decodePath[Discovery](f).fold(throw _, identity)
    },
    Compile / discoveryGenerate / fileInputs ++= (Compile / unmanagedSourceDirectories).value
      .map(_.getParentFile.toGlob / "discovery" / "*.json"),
    Compile / discoveryGenerate := {
      val discovery = (Compile / discoveryDocument).value
      val mangedDir = (Compile / sourceManaged).value / "scala"
      val packageName = discoveryPackage.value

      Codegen.generateFromDiscovery(packageName, discovery).map(_.writeTo(mangedDir.toPath).toFile)
    }
  )
}
