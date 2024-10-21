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
          val wrap = Doc.text("List(") + p.literal + Doc.text(" -> ") + mapper + Doc.text(")")
          if (p.required) wrap
          else {
            wrap + Doc.text(".flatMap{ case (k, v) => v.map(vv => k -> Option(vv)) }")
          }
        }
      )
      .tightBracketBy(Doc.text("Query.fromVector(Vector("), Doc.text(").flatten)"))
  }
}
