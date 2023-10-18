package discovery

import io.circe.Decoder
import io.circe.generic.semiauto.*

case class Discovery(schemas: Map[String, Schema], revision: String)
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
