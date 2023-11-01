package discovery

import cats.Traverse
import cats.data.Writer
import sbt._
import org.typelevel.paiges.Document.ops._

object Codegen {
  case class SourceFile(pkg: String, name: String, imports: List[String], body: String) {

    def render =
      s"""|package $pkg
          |
          |${imports.map("import " + _).mkString("\n")}
          |
          |$body
          |""".stripMargin

    def writeTo(basedir: File) = {
      val packageDir = pkg.split("\\.").foldLeft(basedir)(_ / _)
      val file = packageDir / s"${name}.scala"
      IO.createDirectory(packageDir)
      IO.write(file, render)
      file
    }
  }

  def generateFromDiscovery(packageName: String, discovery: Discovery) = {
    val instances = Codegen.jsonInstances(packageName)
    val clients = Client
      .clientsFrom(discovery)
      .map(c =>
        Codegen.SourceFile(
          packageName,
          c.name,
          c.imports,
          c.toCode
        ))

    val models = discovery.schemas
      .filterKeys(!Set("JsonObject", "JsonValue").contains(_))
      .toList
      .flatMap { case (name, schema) =>
        Codegen.mkSchema(name, schema)
      }
      .map { generatedType =>
        Codegen.SourceFile(
          packageName,
          generatedType.name,
          generatedType.imports,
          generatedType.doc.render(80)
        )
      }
    instances :: models ::: clients
  }

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
      property: Schema): Writer[List[GeneratedType], Type] = {

    val primitive = property.format
      .collect {
        case "int32" => Type.simple("Int")
        case "int64" | "uint32" =>
          if (property.description.exists(_.contains("millis")))
            Type.importedType("_root_.scala.concurrent.duration.FiniteDuration")
          else
            Type.simple("Long")
        case "uint64" => Type.simple("BigInt")
        case "double" => Type.simple("Double")
        case "byte" => Type.importedType("_root_.scodec.bits.ByteVector")
      }
      .orElse(property.`type`.filter(_ => property.`enum`.isEmpty).collect {
        case "string" => Type.simple("String")
        case "boolean" => Type.simple("Boolean")
        case "integer" => Type.simple("Int")
        case "number" => Type.simple("Double")
      })
      .map(Writer(List.empty[GeneratedType], _))

    val array = property.`type`.collect { case "array" =>
      property.items.map { p =>
        mkPropertyType(parentName, inflector.singularize(name).capitalize, p).map { t =>
          Type.list(t)
        }
      }
    }.flatten

    val ref = property.`$ref`.map(_.split("/").last)
      .map {
        case "JsonValue" => Type.importedType("_root_.io.circe.Json")
        case "JsonObject" => Type.importedType("_root_.io.circe.JsonObject")
        case x => Type.simple(x)
      }
      .map(Writer(List.empty[GeneratedType], _))

    val obj = property.`type`.collect { case "object" =>
      property.properties
        .map { p =>
          val schemaName = s"$parentName${name.capitalize}"
          Writer(
            mkSchema(schemaName, Schema(properties = Some(p))),
            Type.simple(schemaName)
          )
        }
        .orElse(property.additionalProperties.map { p =>
          mkPropertyType(parentName, name.capitalize, p).map(t =>
            Type.simple(s"Map[String, ${t.name}]")) // todo: need collectionType here
        })
        .getOrElse(
          Writer(List.empty[GeneratedType], Type.importedType("_root_.io.circe.JsonObject")))
    }

    val enumType: Option[Writer[List[GeneratedType], Type]] = property.`enum`.map { enums =>
      val typeName = s"$parentName${name.capitalize}"
      Writer(
        EnumType(typeName, enums, property.enumDescriptions.getOrElse(Nil)) :: Nil,
        Type.simple(typeName))
    }

    primitive
      .orElse(enumType)
      .orElse(ref)
      .orElse(array)
      .orElse(obj)
      .getOrElse(Writer(List.empty[GeneratedType], Type.importedType("_root_.io.circe.Json")))
  }

  def jsonInstances(packageName: String) = SourceFile(
    packageName,
    "JsonInstances",
    List("io.circe._", "scala.concurrent.duration._", "scodec.bits._"), {
      val last = packageName.lastIndexOf('.')
      val simpleName = if (last != -1) packageName.substring(last + 1) else packageName

      s"""|private[$simpleName] object JsonInstances {
        |  implicit val durationEncoder: Encoder[FiniteDuration] = Encoder[Long].contramap(_.toMillis)
        |  implicit val durationDecoder: Decoder[FiniteDuration] = Decoder[Long].map(_.millis)
        |
        |  implicit val byteVectorEncoder: Encoder[ByteVector] = Encoder[String].contramap(_.toBase64)
        |  implicit val byteVectorDecoder: Decoder[ByteVector] = Decoder[String].emap(bv => ByteVector.fromBase64Descriptive(bv))
        |}
        |""".stripMargin
    }
  )
}
