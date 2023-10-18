package discovery

import cats.data.Writer
import cats.syntax.all._
import cats.instances.list._
import io.circe._
import io.circe.generic.auto._
import sbt.Keys._
import sbt._
import sbt.nio._
import sbt.nio.Keys._
import cats.Traverse

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
      val packageDir = packageName.split("\\.").foldLeft(mangedDir)(_ / _)
      IO.createDirectory(packageDir)

      val instances = packageDir / "JsonInstances.scala"
      IO.write(instances, jsonInstances(packageName))

      val models = discovery.schemas
        .filterKeys(!Set("JsonObject", "JsonValue").contains(_))
        .toList
        .flatMap { case (name, schema) =>
          mkSchema(name, schema)
        }
        .map { caseClass =>
          val f =
            packageDir / s"${caseClass.name}.scala"
          val generatedClass =
            s"""|package $packageName
                |
                |import JsonInstances._
                |${caseClass.imports.mkString("\n")}
                |
                |$caseClass
                |""".stripMargin

          IO.write(
            f,
            generatedClass
          )
          f
        }

      instances :: models
    }
  )

  def mkSchema(name: String, schema: Schema): List[GeneratedType] = {
    type F[A] = Writer[List[GeneratedType], A]
    Traverse[List]
      .traverse[F, (String, Schema), Parameter](schema.properties.toList.flatMap(_.toList)) {
        case (propertyName, property) =>
          mkProperty(name, propertyName, property)
      }
      .flatMap(parameters => Writer.tell(CaseClass(name, parameters) :: Nil))
      .written
  }

  def mkProperty(
      parentName: String,
      name: String,
      property: Schema): Writer[List[GeneratedType], Parameter] =
    mkPropertyType(parentName, name, property).map(t =>
      Parameter(name, t, property.description, required = false))

  def mkPropertyType(
      parentName: String,
      name: String,
      property: Schema): Writer[List[GeneratedType], ParamType] = {

    val primitive = property.format
      .collect {
        case "int32" => ParamType.simple("Int")
        case "int64" | "uint32" =>
          if (property.description.exists(_.contains("millis")))
            ParamType.importedType("_root_.scala.concurrent.duration.FiniteDuration")
          else
            ParamType.simple("Long")
        case "uint64" => ParamType.simple("BigInt")
        case "double" => ParamType.simple("Double")
        case "byte" => ParamType.importedType("_root_.scodec.bits.ByteVector")
      }
      /*.orElse(property.`enum`.map(enums =>
        ParamType.enumType(s"${parentName}.${inflector.capitalize(name)}", enums)))*/
      .orElse(property.`type`.filter(_ => property.`enum`.isEmpty).collect {
        case "string" => ParamType.simple("String")
        case "boolean" => ParamType.simple("Boolean")
        case "integer" => ParamType.simple("Int")
        case "number" => ParamType.simple("Double")
      })
      .map(Writer(List.empty[GeneratedType], _))

    val array = property.`type`.collect { case "array" =>
      property.items.map { p =>
        mkPropertyType(parentName, inflector.singularize(name).capitalize, p).map { t =>
          ParamType.list(t)
        }
      }
    }.flatten

    val ref = property.`$ref`.map(_.split("/").last)
      .map {
        case "JsonValue" => ParamType.importedType("_root_.io.circe.Json")
        case "JsonObject" => ParamType.importedType("_root_.io.circe.JsonObject")
        case x => ParamType.simple(x)
      }
      .map(Writer(List.empty[GeneratedType], _))

    val obj = property.`type`.collect { case "object" =>
      property.properties
        .map { p =>
          val schemaName = s"$parentName${name.capitalize}"
          Writer(
            mkSchema(schemaName, Schema(properties = Some(p))),
            ParamType.simple(schemaName)
          )
        }
        .orElse(property.additionalProperties.map { p =>
          mkPropertyType(parentName, name.capitalize, p).map(t =>
            ParamType.simple(s"Map[String, ${t.name}]")) // todo: need collectionType here
        })
        .getOrElse(
          Writer(List.empty[GeneratedType], ParamType.importedType("_root_.io.circe.JsonObject")))
    }

    val enumType: Option[Writer[List[GeneratedType], ParamType]] = property.`enum`.map { enums =>
      val typeName = s"$parentName${name.capitalize}"
      Writer(EnumType(typeName, enums) :: Nil, ParamType.simple(typeName))
    }

    primitive
      .orElse(enumType)
      .orElse(ref)
      .orElse(array)
      .orElse(obj)
      .getOrElse(Writer(List.empty[GeneratedType], ParamType.importedType("_root_.io.circe.Json")))
  }

  def jsonInstances(packageName: String) = {
    val last = packageName.lastIndexOf('.')
    val simpleName = if (last != -1) packageName.substring(last + 1) else packageName

    s"""|package $packageName
        |
        |import io.circe._
        |import scala.concurrent.duration._
        |import scodec.bits._
        |
        |private[$simpleName] object JsonInstances {
        |  implicit val durationEncoder: Encoder[FiniteDuration] = Encoder[Long].contramap(_.toMillis)
        |  implicit val durationDecoder: Decoder[FiniteDuration] = Decoder[Long].map(_.millis)
        |
        |  implicit val byteVectorEncoder: Encoder[ByteVector] = Encoder[String].contramap(_.toBase64)
        |  implicit val byteVectorDecoder: Decoder[ByteVector] = Decoder[String].emap(bv => ByteVector.fromBase64Descriptive(bv))
        |}
        |""".stripMargin
  }
}
