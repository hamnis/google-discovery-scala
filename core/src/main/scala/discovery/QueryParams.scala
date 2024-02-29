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
    Doc
      .intercalate(
        Doc.comma + Doc.lineOrSpace,
        params.map { p =>
          val mapper = p.`type`.asString match {
            case "String" => Doc.text("query.") + p.term
            case _ =>
              Doc.text("query.") + p.term + Doc.text(
                ".map(s => QueryParamEncoder[") + p.`type`.asDoc + Doc.text("].encode(s).value)")
          }
          p.literal + Doc.text(" -> ") + mapper
        }
      )
      .tightBracketBy(Doc.text("Query("), Doc.text(")"))
  }
}
