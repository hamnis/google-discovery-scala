package discovery

import io.circe.jawn

import java.nio.channels.Channels

class DiscoveryTest extends munit.FunSuite {
  test("can decode Discovery") {
    val resource = getClass.getResourceAsStream("/bigquery.json")
    val decoded = jawn.decodeChannel[Discovery](Channels.newChannel(resource))
    assert(decoded.isRight)
  }
}
