package discovery

import io.circe.jawn

import java.nio.channels.Channels

class DiscoveryTest extends munit.FunSuite {
  test("can decode bigquery") {
    val resource = getClass.getResourceAsStream("/bigquery.json")
    val decoded = jawn.decodeChannel[Discovery](Channels.newChannel(resource))
    val discovery = decoded.fold(err => fail(err.getMessage), identity)
    Codegen.generateFromDiscovery("bq", discovery)
  }

  test("can decode storage") {
    val resource = getClass.getResourceAsStream("/storage.json")
    val decoded = jawn.decodeChannel[Discovery](Channels.newChannel(resource))
    val discovery = decoded.fold(err => fail(err.getMessage), identity)
    Codegen.generateFromDiscovery("storage", discovery)
  }

  test("can decode firebase") {
    val resource = getClass.getResourceAsStream("/firebase.json")
    val decoded = jawn.decodeChannel[Discovery](Channels.newChannel(resource))
    val discovery = decoded.fold(err => fail(err.getMessage), identity)
    Codegen.generateFromDiscovery("firebase", discovery)
  }
}
