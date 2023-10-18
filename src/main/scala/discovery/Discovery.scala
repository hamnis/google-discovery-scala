package discovery

import io.circe.Decoder
import io.circe.generic.semiauto.*
case class Discovery(schemas: Map[String, Schema], revision: String, resources: Option[Resources])

object Discovery {
  implicit val decoder: Decoder[Discovery] = deriveDecoder
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
  implicit val decoder: Decoder[HttpParameters] = Decoder.instance(c =>
    for {
      map <- c.as[Map[String, HttpParameter]]
      order <- c.get[Option[List[String]]]("parameterOrder")
    } yield HttpParameters(map, order.getOrElse(Nil)))
}

case class Http(
    path: String,
    httpMethod: String,
    description: String,
    parameters: HttpParameters,
    scopes: List[String],
    request: Option[Schema],
    response: Option[Schema]
)

object Http {
  implicit val decoder: Decoder[Http] = deriveDecoder
}

case class Methods(methods: Map[String, Http])
object Methods {
  implicit val decoder: Decoder[Methods] = Decoder[Map[String, Http]].map(apply)
}

case class Resource(methods: Map[String, Methods])

object Resource {
  implicit val decoder: Decoder[Resource] = Decoder[Map[String, Methods]].map(apply)
}

case class Resources(resources: Map[String, Resource])

object Resources {
  implicit val decoder: Decoder[Resources] = Decoder[Map[String, Resource]].map(apply)
}
