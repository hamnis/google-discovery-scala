package discovery

case class QueryParams(basename: String, params: List[Parameter]) {
  val typeName = basename + "Params"
  // val asType = Type(typeName)

  def isEmpty = params.isEmpty

  def caseClass =
    if (params.isEmpty) Some(CaseClass(typeName, params)) else None
}
