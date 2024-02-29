package discovery

import cats.Traverse
import cats.data.Writer
import org.typelevel.paiges.Doc
import org.typelevel.paiges.Document.ops._

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.collection.compat._

object Codegen {
  case class SourceFile(pkg: String, name: String, imports: List[String], body: String) {

    def render =
      s"""|package $pkg
          |
          |${imports.map("import " + _).mkString("\n")}
          |
          |$body
          |""".stripMargin

    def writeTo(basedir: Path) = {
      val packageDir = pkg.split("\\.").foldLeft(basedir)(_ resolve _)
      val file = packageDir.resolve(s"${name}.scala")
      Files.createDirectories(packageDir)
      Files.write(file, render.getBytes(StandardCharsets.UTF_8))
      file
    }
  }

  def generateFromDiscovery(packageName: String, discovery: Discovery) = {
    val static = Codegen.jsonInstances(packageName) :: Codegen.abstractClient(
      packageName) :: Codegen.googleError(packageName) :: Nil
    val clients = ClientCodegen
      .clientsFrom(discovery)
      .map(c =>
        Codegen.SourceFile(
          packageName,
          c.name,
          c.imports,
          c.toCode
        ))

    val models = discovery.schemas.view
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
    static ::: models ::: clients
  }

  def mkSchema(name: String, schema: Schema): List[GeneratedType] = {
    type F[A] = Writer[List[GeneratedType], A]
    Traverse[List]
      .traverse[F, (String, Schema), Parameter](schema.properties.toList.flatMap(_.toList)) {
        case (propertyName, property) =>
          mkSchemaProperty(name, propertyName, property)
      }
      .flatMap(parameters => Writer.tell(CaseClass(name, parameters) :: Nil))
      .written
  }

  def mkSchemaProperty(
      parentName: String,
      name: String,
      property: Schema): Writer[List[GeneratedType], Parameter] =
    mkSchemaPropertyType(parentName, name, property).map { t =>
      Parameter(
        name,
        t,
        property.description,
        required = false,
        default = Some(Doc.text("None"))
      )
    }

  def mkSchemaPropertyType(
      parentName: String,
      name: String,
      property: Schema): Writer[List[GeneratedType], Type] = {

    val primitive = property.format
      .collect {
        case "int32" => Type("Int")
        case "int64" | "uint32" =>
          if (property.description.exists(_.contains("millis")))
            Type.importedType("scala.concurrent.duration.FiniteDuration")
          else
            Type("Long")
        case "uint64" => Type("BigInt")
        case "double" => Type("Double")
        case "byte" => Type.importedType("scodec.bits.ByteVector")
      }
      .orElse(property.`type`.filter(_ => property.`enum`.isEmpty).collect {
        case "string" => Type("String")
        case "boolean" => Type("Boolean")
        case "integer" => Type("Int")
        case "number" => Type("Double")
      })
      .map(Writer(List.empty[GeneratedType], _))

    val array = property.`type`.collect { case "array" =>
      property.items.map { p =>
        mkSchemaPropertyType(parentName, inflector.singularize(name).capitalize, p).map { t =>
          Type.list(t)
        }
      }
    }.flatten

    val ref = property.`$ref`.map(_.split("/").last)
      .map {
        case "JsonValue" => Type.apply("Json")
        case "JsonObject" => Type.apply("JsonObject")
        case x => Type.apply(x)
      }
      .map(Writer(List.empty[GeneratedType], _))

    val obj = property.`type`.collect { case "object" =>
      property.properties
        .map { p =>
          val schemaName = s"$parentName${name.capitalize}"
          Writer(
            mkSchema(schemaName, Schema(properties = Some(p))),
            Type.apply(schemaName)
          )
        }
        .orElse(property.additionalProperties.map { p =>
          mkSchemaPropertyType(parentName, name.capitalize, p).map(t =>
            Type.map(Type.apply("String"), t))
        })
        .getOrElse(Writer(List.empty[GeneratedType], Type.apply("JsonObject")))
    }

    val enumType: Option[Writer[List[GeneratedType], Type]] = property.`enum`.map { enums =>
      val typeName = s"$parentName${name.capitalize}"
      Writer(
        EnumType(typeName, enums, property.enumDescriptions.getOrElse(Nil)) :: Nil,
        Type.apply(typeName))
    }

    primitive
      .orElse(enumType)
      .orElse(ref)
      .orElse(array)
      .orElse(obj)
      .getOrElse(Writer(List.empty[GeneratedType], Type.apply("Json")))
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

  def abstractClient(packageName: String): SourceFile = SourceFile(
    packageName,
    "AbstractClient",
    List(
      "cats.syntax.all._",
      "cats.effect.Concurrent",
      "io.circe._",
      "org.http4s._",
      "org.http4s.client.Client"),
    """abstract class AbstractClient[F[_]](client: Client[F])(implicit
      |    F: Concurrent[F]) {
      |  private implicit def entityDecoder[A: Decoder]: EntityDecoder[F, A] =
      |    org.http4s.circe.jsonOf[F, A]
      |  private implicit def entityEncoder[A: Encoder]: EntityEncoder[F, A] =
      |    org.http4s.circe.jsonEncoderOf[F, A]
      |
      |  protected def request(uri: Uri, method: Method) =
      |    Request[F](uri = uri, method = method)
      |
      |  protected def requestWithBody[A: Encoder](uri: Uri, method: Method)(input: A) =
      |    Request[F](uri = uri, method = method).withEntity(input)
      |
      |  def expectJson[A: Decoder](req: Request[F]) =
      |    client.expectOr[A](req) { res =>
      |      res
      |        .as[GoogleError]
      |        .attempt
      |        .flatMap(err =>
      |          F.raiseError(
      |            err
      |              .fold(
      |                err => GoogleError(Some(res.status.code), Option(err.getMessage), Nil, Nil),
      |                identity)
      |          ))
      |    }
      |}
      |
      |""".stripMargin
  )

  def googleError(packageName: String): SourceFile = SourceFile(
    packageName,
    "GoogleError",
    List("io.circe.Decoder"),
    """case class GoogleError(
      |    code: Option[Int],
      |    message: Option[String],
      |    errors: List[GoogleError.ErrorInfo],
      |    details: List[GoogleError.Details]
      |) extends Exception(message.getOrElse(""))
      |
      |object GoogleError {
      |  final case class ErrorInfo(
      |      domain: Option[String],
      |      reason: Option[String],
      |      message: Option[String],
      |      location: Option[String],
      |      locationType: Option[String]
      |  )
      |
      |  object ErrorInfo {
      |    implicit val decoder: Decoder[ErrorInfo] =
      |      Decoder.forProduct5("domain", "reason", "message", "location", "locationType")(apply)
      |  }
      |
      |  final case class Details(
      |      `type`: Option[String],
      |      reason: Option[String],
      |      parameterViolations: List[ParameterViolation])
      |  object Details {
      |    implicit val decoder: Decoder[Details] = Decoder.instance(c =>
      |      for {
      |        t <- c.get[Option[String]]("@type")
      |        r <- c.get[Option[String]]("reason")
      |        v <- c.get[Option[List[ParameterViolation]]]("parameterViolations")
      |      } yield Details(t, r, v.getOrElse(Nil)))
      |  }
      |
      |  final case class ParameterViolation(parameter: Option[String], description: Option[String])
      |
      |  object ParameterViolation {
      |    implicit val decoder: Decoder[ParameterViolation] =
      |      Decoder.forProduct2("parameter", "description")(apply)
      |  }
      |
      |  implicit val decoder: Decoder[GoogleError] = Decoder
      |    .instance(c =>
      |      for {
      |        code <- c.get[Option[Int]]("code")
      |        message <- c.get[Option[String]]("message")
      |        errors <- c.get[Option[List[ErrorInfo]]]("errors")
      |        details <- c.get[Option[List[Details]]]("details")
      |      } yield GoogleError(code, message, errors.getOrElse(Nil), details.getOrElse(Nil)))
      |    .at("error")
      |}
      |""".stripMargin
  )
}
