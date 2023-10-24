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

}
