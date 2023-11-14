package discovery

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

trait SourceFilePlatform {
  def name: String
  def pkg: String
  def render: String

  def writeTo(basedir: Path) = {
    val packageDir = pkg.split("\\.").foldLeft(basedir)(_ resolve _)
    val file = packageDir.resolve(s"${name}.scala")
    Files.createDirectories(packageDir)
    Files.write(file, render.getBytes(StandardCharsets.UTF_8))
    file
  }
}
