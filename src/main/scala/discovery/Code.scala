package discovery

import org.typelevel.paiges.{Doc, Document}

object Code {
  val lparens = Doc.char('(')
  val rparens = Doc.char(')')
  val lbrace = Doc.char('{')
  val rbrace = Doc.char('}')
  val lbracket = Doc.char('[')
  val rbracket = Doc.char(']')
  val quote = Doc.char('"')

  def interpolate(prefix: String, value: Doc) =
    Doc.text(s"$prefix") + literal(value)

  def interpolatedValue(string: String) =
    Doc.text("${") + Doc.text(string) + rbrace

  def lit(value: String) = literal(Doc.text(value))

  def literal(doc: Doc) = quote + doc + quote

  def assigment(n: Doc, arg: Doc) =
    n + Doc.space + Doc.char('=') + Doc.space + arg

  def ascribed(expr: Doc, typ: Doc) = expr + Doc.char(':') + Doc.space + typ

  def block(expr: Doc) = expr.tightBracketBy(lbrace + Doc.lineOrEmpty, Doc.lineOrEmpty + rbrace)

  def blocks(exprs: List[Doc]) = {
    val expr = Doc.intercalate(Doc.lineBreak, exprs)
    block(expr)
  }

  def term(name: String) = Doc.text(Sanitize(name))

  object Sanitize {
    def apply(s: String): String = s match {
      case "type" => "`type`"
      case s => s
    }
  }

}
