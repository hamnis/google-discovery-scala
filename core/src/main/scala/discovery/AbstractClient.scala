package discovery

import cats.syntax.all._
import cats.effect.Concurrent
import io.circe.{Decoder, Encoder}
import org.http4s._

class AbstractClient[F[_]](client: org.http4s.client.Client[F])(implicit F: Concurrent[F]) {
  private implicit def entityDecoder[A: Decoder]: EntityDecoder[F, A] =
    org.http4s.circe.jsonOf[F, A]
  private implicit def entityEncoder[A: Encoder]: EntityEncoder[F, A] =
    org.http4s.circe.jsonEncoderOf[F, A]

  protected def request(uri: Uri, method: Method) =
    Request[F](uri = uri, method = method)

  protected def requestWithBody[A: Encoder](uri: Uri, method: Method)(input: A) =
    Request[F](uri = uri, method = method).withEntity(input)

  def expectJson[A: Decoder](req: Request[F]) =
    client.expectOr[A](req)(res => res.as[GoogleError].flatMap(err => F.raiseError(err)))
}
