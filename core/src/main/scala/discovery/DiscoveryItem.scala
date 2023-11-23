package discovery

import io.circe.{Decoder, DecodingFailure}
import org.http4s.Uri

case class DiscoveryCollection(discoveryVersion: String, items: Vector[DiscoveryCollection.Item])

object DiscoveryCollection {

  case class Item(
      id: String,
      name: String,
      version: String,
      description: String,
      discoveryRestUrl: Uri,
      documentationLink: Option[Uri],
      preferred: Boolean)

  object Item {
    private implicit val decodeUri: Decoder[Uri] =
      Decoder.decodeString.emapTry(uri => Uri.fromString(uri).toTry)

    implicit val decoder: Decoder[Item] = Decoder.instance { c =>
      for {
        _ <- c
          .get[String]("kind")
          .filterOrElse(_ == "discovery#directoryItem", DecodingFailure("Wrong kind", c.history))
        id <- c.get[String]("id")
        name <- c.get[String]("name")
        version <- c.get[String]("version")
        description <- c.get[String]("description")
        discoveryRestUrl <- c.get[Uri]("discoveryRestUrl")
        documentationLink <- c.get[Option[Uri]]("documentationLink")
        preferred <- c.get[Boolean]("preferred")
      } yield Item(id, name, version, description, discoveryRestUrl, documentationLink, preferred)
    }
  }

  implicit val decoder: Decoder[DiscoveryCollection] =
    Decoder.forProduct2("discoveryVersion", "items")(apply)

}
