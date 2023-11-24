package discovery

import cats.effect.kernel.Concurrent
import discovery.DiscoveryClient.AsJson
import io.circe.{Decoder, Encoder, Json}
import org.http4s.{EntityDecoder, Request, Uri}
import org.http4s.client.{Client => Http4sClient}
import org.http4s.implicits._

class DiscoveryClient[F[_]: Concurrent](client: Http4sClient[F]) {
  implicit def entityDecoder[A](implicit d: Decoder[A]): EntityDecoder[F, A] =
    org.http4s.circe.jsonOf

  def getCollection: F[DiscoveryCollection] =
    client.expect[DiscoveryCollection](
      Request[F](uri = uri"https://discovery.googleapis.com/discovery/v1/apis"))

  def getDiscovery(item: DiscoveryCollection.Item): F[Option[AsJson[Discovery]]] =
    getDiscoveryByUri(item.discoveryRestUrl)

  def getDiscoveryByUri(uri: Uri): F[Option[AsJson[Discovery]]] =
    client.expectOption[AsJson[Discovery]](Request[F](uri = uri))
}

object DiscoveryClient {
  case class AsJson[A](value: A, json: Json)
  object AsJson {
    implicit def decoder[A](implicit d: Decoder[A]): Decoder[AsJson[A]] =
      Decoder.instance(c =>
        for {
          json <- c.as[Json]
          value <- json.as[A](d)
        } yield AsJson(value, json))

    implicit def encoder[A]: Encoder[AsJson[A]] = Encoder.encodeJson.contramap(_.json)
  }
}
