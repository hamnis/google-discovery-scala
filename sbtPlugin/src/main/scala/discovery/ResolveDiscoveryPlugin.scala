package discovery

import io.circe.jawn.parseChannel
import sbt.*
import Keys.*

import java.net.HttpURLConnection
import java.nio.channels.Channels

object ResolveDiscoveryPlugin extends AutoPlugin {
  import DiscoveryPlugin.autoImport._

  override def trigger = NoTrigger
  val discoveryCollectionUri = url("https://discovery.googleapis.com/discovery/v1/apis")

  object autoImport {
    lazy val discoveryUri = settingKey[URL](
      "uri for discovery document. e.g: url(\"https://bigquery.googleapis.com/discovery/v1/apis/bigquery/v2/rest\")")
    lazy val discoveryFetch =
      taskKey[Unit]("fetches the url and saves it to the path configured in discoveryDirectory")

    lazy val discoveryList =
      taskKey[DiscoveryCollection](
        s"Fetches and parses the discovery document list found at ${discoveryCollectionUri.toExternalForm}")
  }
  import autoImport._

  override def buildSettings: Seq[Def.Setting[_]] =
    discoveryList := {
      def decode(file: File) = _root_.io.circe.jawn
        .decodeFile[DiscoveryCollection](file)
        .fold(throw _, identity)

      val targetDir = (LocalRootProject / target).value

      val cache = targetDir / "collection.json"
      if (!cache.exists()) {
        val conn = discoveryCollectionUri.openConnection()
        conn.connect()
        conn match {
          case connection: HttpURLConnection =>
            if (connection.getResponseCode == 200) {
              val channel = Channels.newChannel(connection.getInputStream)
              parseChannel(channel).fold(
                err => sys.error(Option(err.getMessage()).getOrElse("Unable to parse")),
                json => IO.write(cache, json.spaces2SortKeys))
            } else
              sys.error(
                s"Did not find the discovery document, error code ${connection.getResponseCode}")
          case _ => sys.error("unhandled url connection")
        }
      }
      if (cache.exists()) {
        decode(cache)
      } else {
        sys.error("No cached document found")
      }
    }

  override def projectSettings: Seq[Def.Setting[_]] = Seq(
    discoveryFetch := {
      val uri = discoveryUri.value
      val path = (Compile / discoveryFile).value
      val log = streams.value.log
      val conn = uri.openConnection()
      conn.connect()
      conn match {
        case connection: HttpURLConnection =>
          if (connection.getResponseCode == 200) {
            IO.createDirectory(path.getParentFile)
            val channel = Channels.newChannel(connection.getInputStream)
            parseChannel(channel).fold(
              err => sys.error(Option(err.getMessage()).getOrElse("Unable to parse")),
              json => IO.write(path, json.spaces2SortKeys))
            log.info(s"Wrote ${uri.toString} to $path")
          }
        case _ => sys.error("unhandled url connection")
      }
    }
  )
}
