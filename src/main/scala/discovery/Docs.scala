package discovery

import org.typelevel.paiges.{Doc, Document}

object Docs {
  val lparens = Doc.text("(")
  val rparens = Doc.text(")")
  val lbrace = Doc.text("{")
  val rbrace = Doc.text("}")
  val lbracket = Doc.text("[")
  val rbracket = Doc.text("]")
  val quote = Doc.text("\"")

  implicit class MyExt(val doc: Doc.type) extends AnyVal {
    def renderDoc[A: Document](a: A): Doc = Document[A].document(a)
  }

  def interpolate(prefix: String, value: Doc) =
    Doc.text(s"$prefix") + literal(value)

  def interpolatedValue(string: String) =
    Doc.text("${") + Doc.text(string) + rbrace

  def lit(value: String) = literal(Doc.text(value))

  def literal(doc: Doc) = quote + doc + quote

  def assigment(n: Doc, arg: Doc) =
    n + Doc.space + Doc.char('=') + Doc.space + arg

  def ascribed(expr: Doc, typ: Doc) = expr + Doc.char(':') + Doc.space + typ

  def block(expr: Doc) = expr.tightBracketBy(lbrace + Doc.lineOrSpace, Doc.hardLine + rbrace)

  def blocks(exprs: List[Doc]) = {
    val expr = Doc.intercalate(Doc.hardLine, exprs) + Doc.hardLine
    block(expr)
  }

}
