package discovery

import io.circe.*
import io.circe.generic.auto.*
import sbt.Keys.*
import sbt.*
import sbt.nio.Keys.*

object DiscoveryPlugin extends AutoPlugin {

  override def trigger = noTrigger

  object autoImport extends scala.AnyRef {
    lazy val discoveryGenerate =
      taskKey[Seq[File]]("Generate Scala case classes for the given Discovery Document")
    lazy val discoveryPackage = settingKey[String]("Package for generated sources")
  }
  import autoImport._

  override val projectSettings = Seq(
    Compile / sourceGenerators += Compile / discoveryGenerate,
    Compile / packageSrc / mappings ++= {
      val base = (Compile / sourceManaged).value
      val files = (Compile / managedSources).value
      files.map(f => (f, f.relativeTo(base).get.getPath))
    },
    Compile / discoveryGenerate / fileInputs ++= (Compile / unmanagedSourceDirectories).value
      .map(_.getParentFile.toGlob / "discovery" / "*.json"),
    Compile / discoveryGenerate := {
      val f = (Compile / discoveryGenerate).inputFiles.head
      val discovery = jawn.decodePath[Discovery](f).fold(throw _, identity)
      val mangedDir = (Compile / sourceManaged).value / "scala"
      val packageName = discoveryPackage.value

      Codegen.generateFromDiscovery(packageName, discovery).map(_.writeTo(mangedDir))
    }
  )
}
