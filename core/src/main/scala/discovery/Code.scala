package discovery

import org.typelevel.paiges.Doc
import org.typelevel.paiges.Document.ops._

import java.util.Scanner

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

  def paramsToDoc(p: List[Parameter]) =
    if (p.nonEmpty)
      Doc
        .intercalate(Doc.comma + Doc.lineOrSpace, p.map(_.doc))
        .tightBracketBy(Code.lparens, Code.rparens)
    else Doc.empty

  def Def(
      name: String,
      tparams: List[Type],
      params: List[Parameter],
      returnType: Option[Type],
      body: Doc) = {
    val defn = Doc.text(s"def ") + term(name)
    val appliedTParams =
      if (tparams.nonEmpty)
        Doc
          .intercalate(Doc.comma + Doc.lineOrSpace, tparams.map(_.asDoc))
          .tightBracketBy(Code.lbracket, Code.rbracket)
      else Doc.empty

    val appliedParams = paramsToDoc(params)

    val applied = defn + appliedTParams + appliedParams
    assigment(
      returnType.map(t => ascribed(applied, t.asDoc)).getOrElse(applied),
      Doc.lineOrEmpty + body)
  }

  def blockComment(s: String) = {
    val buf = List.newBuilder[String]
    val scanner = new Scanner(s)
    scanner.useDelimiter("\\n")
    while (scanner.hasNext()) {
      val line = scanner.next()
      val lines = if (line.contains(" * ")) {
        line.split(" \\* ").toList.map(_.replaceAll("\\*", "\\\\*"))
      } else List(line)

      buf ++= lines
    }

    val parts =
      Doc.foldDocs(buf.result().map(s => Doc.text("* " + s)))((x, y) => x + (Doc.hardLine + y))
    parts
      .tightBracketBy(
        Doc.hardLine + Doc.text("/**") + Doc.hardLine,
        Doc.hardLine + Doc.text("*/") + Doc.hardLine,
        0
      )
  }

  def term(name: String) = Doc.text(Sanitize(name))

  def termSelect(name: Doc, selected: Doc) =
    name + Doc.lineOrEmpty + Doc.char('.') + Doc.lineOrEmpty + selected

  def termApply(name: Doc, typeParams: List[Type], arg: Doc) = {
    val tApplied =
      if (typeParams.nonEmpty)
        Doc.intercalate(Doc.comma, typeParams.map(_.asDoc)).tightBracketBy(lbracket, rbracket)
      else Doc.empty
    arg.tightBracketBy(name + tApplied + lparens, rparens)
  }

  def forComprehension(generators: List[(Doc, Doc)], yieldBlock: Doc) = {
    val genDoc = Doc.hardLine + Doc.intercalate(
      Doc.hardLine,
      generators.map { case (pat, gen) =>
        pat + Doc.lineOrSpace + Doc.text("<-") + Doc.lineOrSpace + gen
      })

    val yieldDoc = Doc.hardLine + rbrace + Doc.text(" yield ") + yieldBlock

    genDoc.tightBracketBy(Doc.text("for {"), yieldDoc)
  }

  object Sanitize {
    def apply(s: String): String = s match {
      case "type" => "`type`"
      case "object" => "`object`"
      case s if s.contains(".") => s.replace('.', '_')
      case s => s
    }
  }

}
