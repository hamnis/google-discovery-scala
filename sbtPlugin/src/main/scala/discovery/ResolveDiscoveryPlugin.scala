package discovery

import io.circe.jawn.*
import sbt.*
import Keys.*

import java.net.HttpURLConnection
import java.nio.file.Path

object ResolveDiscoveryPlugin extends AutoPlugin {
  import DiscoveryPlugin.autoImport._

  override def trigger = NoTrigger
  val discoveryCollectionUri = uri("https://discovery.googleapis.com/discovery/v1/apis")

  object autoImport {
    lazy val discoveryUri = settingKey[URI](
      "uri for discovery document. e.g: uri(\"https://bigquery.googleapis.com/discovery/v1/apis/bigquery/v2/rest\")")
    lazy val discoveryFetch =
      taskKey[Unit]("fetches the url and saves it to the path configured in discoveryDirectory")

    lazy val discoveryList =
      taskKey[DiscoveryCollection](
        s"Fetches and parses the discovery document list found at ${discoveryCollectionUri.toString}")
  }
  import autoImport._

  override def buildSettings: Seq[Def.Setting[?]] =
    discoveryList := Def.uncached {
      val log = streams.value.log

      def decode(file: File) = _root_.io.circe.jawn
        .decodeFile[DiscoveryCollection](file)
        .fold(throw _, identity)

      val targetDir = (LocalRootProject / target).value

      val cache = targetDir / "collection.json"
      if (!cache.exists()) {
        download(discoveryCollectionUri, cache, log)
      }
      if (cache.exists()) {
        decode(cache)
      } else {
        sys.error("No cached document found")
      }
    }

  override def projectSettings: Seq[Def.Setting[?]] = Seq(
    discoveryFetch := Def.uncached {
      val uri = discoveryUri.value
      val path = (Compile / discoveryFile).value
      val log = streams.value.log
      download(uri, path, log)
    }
  )

  private def download(uri: URI, path: File, log: Logger): Unit = {
    val conn = uri.toURL.openConnection()
    conn.connect()
    conn match {
      case connection: HttpURLConnection =>
        if (connection.getResponseCode == 200) {
          IO.createDirectory(path.getParentFile)
          val bytes = IO.readBytes(connection.getInputStream)

          parseByteArray(bytes).fold(
            err => sys.error(Option(err.getMessage()).getOrElse("Unable to parse")),
            json => IO.write(path, json.spaces2SortKeys))
          log.info(s"Wrote ${uri.toString} to $path")
        }
      case _ => sys.error("unhandled url connection")
    }
  }
}
