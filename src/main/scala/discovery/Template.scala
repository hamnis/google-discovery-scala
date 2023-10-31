package discovery

import discovery.Template.Variable
import org.typelevel.paiges.Doc

case class Template(path: String, params: List[Parameter]) {
  def expand(segment: String) = {
    val m = Variable.pattern.matcher(segment)
    if (m.find) Docs.interpolate("s", Docs.interpolatedValue(m.group(2)))
    else Docs.lit(segment)
  }

  def paramsAsDoc = Doc
    .intercalate(Doc.comma + Doc.line, params.map(p => Parameter.renderer.document(p)))

  def toCodeDoc = {
    val segment = Doc.space + Doc.text("/") + Doc.space
    val rendered = Doc.intercalate(segment, path.split("/").map(expand))
    Doc.text("baseUri") + segment + rendered
  }
}

object Template {
  val Variable = "(\\{\\+?([^/]+)})".r
}
