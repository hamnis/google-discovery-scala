package discovery

import cats.effect.IO
import fs2.io.file.{Files, Path}
import org.http4s.implicits._
import org.http4s.{HttpApp, Response}

class DiscoveryClientTest extends munit.CatsEffectSuite {
  val files = Files.forIO

  test("get some") {
    val body = files.readAll(Path("core/src/test/resources/list.json"))
    val c = new DiscoveryClient[IO](
      org.http4s.client.Client.fromHttpApp(HttpApp.liftF(IO(Response[IO](body = body))))
    )
    c.getCollection.map(_.items.size).assertEquals(13)
  }

  test("get one") {
    val c = new DiscoveryClient[IO](
      org.http4s.client.Client.fromHttpApp(HttpApp(r =>
        if (r.uri == uri"https://discovery.googleapis.com/discovery/v1/apis") {
          val body = files.readAll(Path("core/src/test/resources/list.json"))
          IO(Response[IO](body = body))
        } else if (r.uri == uri"https://bigquery.googleapis.com/$$discovery/rest?version=v2") {
          val body = files.readAll(Path("core/src/test/resources/bigquery.json"))
          IO(Response[IO](body = body))
        } else {
          IO(Response.notFound[IO])
        }))
    )
    c.getCollection
      .map(_.items.last)
      .flatMap(item => c.getDiscovery(item))
      .map(opt => assert(opt.isDefined))
  }
}
