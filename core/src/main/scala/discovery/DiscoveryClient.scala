package discovery

import cats.effect.kernel.Concurrent
import io.circe.Decoder
import org.http4s.{EntityDecoder, Request}
import org.http4s.client.{Client => Http4sClient}
import org.http4s.implicits._

class DiscoveryClient[F[_]: Concurrent](client: Http4sClient[F]) {
  implicit def entityDecoder[A](implicit d: Decoder[A]): EntityDecoder[F, A] =
    org.http4s.circe.jsonOf

  def getCollection =
    client.expect[DiscoveryCollection](
      Request[F](uri = uri"https://discovery.googleapis.com/discovery/v1/apis"))

  def getDiscovery(item: DiscoveryCollection.Item) =
    client.expectOption[Discovery](Request[F](uri = item.discoveryRestUrl))
}
