package discovery

import io.circe.Decoder
import io.circe.generic.semiauto._
import org.http4s.Uri

case class Discovery(
    schemas: Map[String, Schema],
    revision: String,
    version: String,
    baseUrl: Uri,
    resources: Resources
)

object Discovery {
  implicit val uriDecoder: Decoder[Uri] =
    Decoder[String].emap(Uri.fromString(_).left.map(_.message))
  implicit val decoder: Decoder[Discovery] = Decoder.instance(c =>
    for {
      schemas <- c.get[Map[String, Schema]]("schemas")
      revision <- c.get[String]("revision")
      version <- c.get[String]("version")
      baseUrl <- c.get[Uri]("baseUrl")
      resources <- c.get[Option[Resources]]("resources")
    } yield Discovery(schemas, revision, version, baseUrl, resources.getOrElse(Resources.empty)))
}

case class Schema(
    description: Option[String] = None,
    `type`: Option[String] = None,
    format: Option[String] = None,
    $ref: Option[String] = None,
    properties: Option[Map[String, Schema]] = None,
    additionalProperties: Option[Schema] = None,
    items: Option[Schema] = None,
    `enum`: Option[List[String]] = None,
    enumDescriptions: Option[List[String]] = None
)

object Schema {
  implicit val decoder: Decoder[Schema] = deriveDecoder
}

case class HttpParameter(
    `type`: String,
    description: String,
    required: Option[Boolean],
    location: String
)
object HttpParameter {
  implicit val decoder: Decoder[HttpParameter] = deriveDecoder
}

case class HttpParameters(parameters: Map[String, HttpParameter], order: List[String])

object HttpParameters {
  val empty = HttpParameters(Map.empty, Nil)

  implicit val decoder: Decoder[HttpParameters] = Decoder.instance(c =>
    for {
      map <- c.as[Map[String, HttpParameter]]
      order <- c.up.get[Option[List[String]]]("parameterOrder")
    } yield HttpParameters(map, order.getOrElse(Nil)))
}

case class ApiMethod(
    path: String,
    httpMethod: String,
    description: Option[String],
    parameters: HttpParameters,
    scopes: List[String],
    request: Option[Schema],
    response: Option[Schema],
    deprecated: Option[Boolean]
)

object ApiMethod {
  implicit val decoder: Decoder[ApiMethod] = Decoder.instance(c =>
    for {
      path <- c.get[String]("path")
      httpMethod <- c.get[String]("httpMethod")
      description <- c.get[Option[String]]("description")
      parameters <- c.get[Option[HttpParameters]]("parameters")
      scopes <- c.get[Option[List[String]]]("scopes")
      request <- c.get[Option[Schema]]("request")
      response <- c.get[Option[Schema]]("response")
      deprecated <- c.get[Option[Boolean]]("deprecated")
    } yield ApiMethod(
      path,
      httpMethod,
      description,
      parameters.getOrElse(HttpParameters.empty),
      scopes.getOrElse(Nil),
      request,
      response,
      deprecated
    ))
}

case class Resource(
    methods: Map[String, ApiMethod],
    resources: Resources,
    deprecated: Option[Boolean]) {
  def get(name: String) = methods.get(name)
}

object Resource {
  implicit val decoder: Decoder[Resource] = Decoder.instance { c =>
    for {
      methods <- c
        .get[Option[Map[String, ApiMethod]]]("methods")
        .map(opt => opt.orElse(c.as[Option[Map[String, ApiMethod]]].toOption.flatten))
      resources <- c.get[Option[Resources]]("resources")
      deprecated <- c.get[Option[Boolean]]("deprecated")
    } yield Resource(methods.getOrElse(Map.empty), resources.getOrElse(Resources.empty), deprecated)
  }
}

case class Resources(resources: Map[String, Resource]) {
  def isEmpty = resources.isEmpty
  def get(name: String) = resources.get(name)
}

object Resources {
  val empty = Resources(Map.empty)
  implicit val decoder: Decoder[Resources] = Decoder[Map[String, Resource]].map(apply)
}
