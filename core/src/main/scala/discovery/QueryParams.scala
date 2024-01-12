package discovery

import org.typelevel.paiges._

case class QueryParams(basename: String, params: List[Parameter]) {
  val typeName = basename + "Params"

  def isEmpty = params.isEmpty
  def nonEmpty = params.nonEmpty

  def caseClass =
    if (params.nonEmpty) Some(CaseClass(typeName, params)) else None

  def toQueryParamsDoc = if (isEmpty) Doc.empty
  else {
    val qpParams = Doc
      .intercalate(
        Doc.comma + Doc.lineOrSpace,
        params.map(p => p.literal + Doc.text(" -> ") + Doc.text("query") + p.term))
      .tightBracketBy(Doc.text("Map("), Doc.text(")"))
    Doc.text("withQueryParams(") + qpParams + Doc.text(")")
  }
}
