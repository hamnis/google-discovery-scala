package discovery

import cats.syntax.all._
import cats.effect.Concurrent
import io.circe.{Decoder, Encoder}
import org.http4s._
import org.http4s.client.Client

abstract class AbstractClient[F[_]](client: Client[F])(implicit F: Concurrent[F]) {
  private implicit def entityDecoder[A: Decoder]: EntityDecoder[F, A] =
    org.http4s.circe.jsonOf[F, A]
  private implicit def entityEncoder[A: Encoder]: EntityEncoder[F, A] =
    org.http4s.circe.jsonEncoderOf[F, A]

  protected def request(uri: Uri, method: Method) =
    Request[F](uri = uri, method = method)

  protected def requestWithBody[A: Encoder](uri: Uri, method: Method)(input: A) =
    Request[F](uri = uri, method = method).withEntity(input)

  def expectJson[A: Decoder](req: Request[F]) =
    client.expectOr[A](req) { res =>
      res
        .as[GoogleError]
        .attempt
        .flatMap(err =>
          F.raiseError(
            err
              .fold(
                err => GoogleError(Some(res.status.code), Option(err.getMessage), Nil, Nil),
                identity)
          ))
    }
}
