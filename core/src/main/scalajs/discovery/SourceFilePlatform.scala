package discovery

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

trait SourceFilePlatform {
  def name: String
  def pkg: String
  def render: String

}
