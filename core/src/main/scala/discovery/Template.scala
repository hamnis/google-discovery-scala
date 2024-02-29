package discovery

import discovery.Template.Variable
import org.typelevel.paiges.Doc

case class Template(path: String, params: List[Parameter]) {
  def expand(segment: String) = {
    val m = Variable.pattern.matcher(segment)
    if (m.find) Code.interpolate("s", Code.interpolatedValue(m.group(2)))
    else Code.lit(segment)
  }

  def paramsAsDoc = Doc
    .intercalate(Doc.comma + Doc.line, params.map(p => Parameter.renderer.document(p)))

  def toCodeDoc(query: QueryParams) = {
    val segment = Doc.space + Doc.text("/") + Doc.space
    val rendered = Doc.intercalate(segment, path.split("/").map(expand))
    val asUri = Doc.text("baseUri") + segment + rendered
    if (query.nonEmpty) {
      Code.lparens + asUri + Code.rparens + Doc.text(
        ".copy(query = ") + query.toQueryParamsDoc + Code.rparens
    } else asUri
  }
}

object Template {
  val Variable = "(\\{\\+?([^/]+)})".r
}
