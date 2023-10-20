package discovery

import discovery.Template.Variable

case class Template(path: String, params: List[Parameter]) {
  def expand(segment: String) = {
    val m = Variable.pattern.matcher(segment)
    if (m.find) Codegen.interpolate("s", Codegen.surroundWith("${", "}")(m.group(2)))
    else Codegen.lit(segment)
  }

  def paramsAsString = s"${params.mkString(", \n")}"

  def toCode(baseUri: String) = {
    val rendered = path.split("/").map(expand).mkString(" / ")
    s"${Codegen.interpolate("uri", baseUri)} / $rendered"
  }
}

object Template {
  val Variable = "(\\{\\+?([^/]+)})".r
}
