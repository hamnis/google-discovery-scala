package discovery

import sbt.*
import Keys.*

import java.net.HttpURLConnection

object ResolveDiscoveryPlugin extends AutoPlugin {
  override def trigger = NoTrigger

  object autoImport {
    lazy val discoveryDirectory = settingKey[File]("directory for saved discovery.json")
    lazy val discoveryUri = settingKey[URL](
      "uri for discovery document. e.g: url(\"https://bigquery.googleapis.com/discovery/v1/apis/bigquery/v2/rest\")")
    lazy val discoveryFetch =
      taskKey[Unit]("fetches the url and saves it to the path configured in discoveryDirectory")
  }
  import autoImport._

  override def projectSettings: Seq[Def.Setting[_]] = Seq(
    discoveryDirectory := baseDirectory.value / "src" / "main" / "discovery" / "discovery.json",
    discoveryFetch := {
      val uri = discoveryUri.value
      val path = discoveryDirectory.value
      val log = streams.value.log
      val conn = uri.openConnection()
      conn.connect()
      conn match {
        case connection: HttpURLConnection =>
          if (connection.getResponseCode == 200) {
            IO.createDirectory(path.getParentFile)
            IO.transfer(connection.getInputStream, path)
            log.info(s"Wrote ${uri.toString} to $path")
          }
        case _ => sys.error("unhandled url connection")
      }

    }
  )
}
