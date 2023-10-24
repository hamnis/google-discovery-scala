package discovery

import org.typelevel.paiges.{Doc, Document}

sealed trait ParamType extends Product with Serializable {
  def name: String
}

object ParamType {
  def simple(in: String): ParamType = SimpleType(in)
  def importedType(in: String): ParamType = ImportedType(in, in.substring(in.lastIndexOf('.') + 1))

  def list(typ: ParamType): ParamType = ListType(typ)
}

case class SimpleType(name: String) extends ParamType
case class ListType(elemType: ParamType) extends ParamType {
  override def name: String = s"List[${elemType.name}]"
}
case class ImportedType(fqcn: String, name: String) extends ParamType

sealed trait GeneratedType {
  def name: String
  def imports: List[String]
}

case class TypeClassInstance(name: String, `type`: ParamType, body: Doc)
object TypeClassInstance {
  implicit val renderer: Document[TypeClassInstance] = Document.instance(tci =>
    Doc.text("implicit val ") + Doc.text(Sanitize(tci.name)) + Doc.space + Doc.char(':') + Doc
      .split(s" ${tci.`type`.name} = ") + tci.body)
}

case class EnumType(name: String, cases: List[String], descriptions: List[String])
    extends GeneratedType {

  override def imports: List[String] = List("io.circe._", "io.circe.syntax._")
}

object EnumType {
  implicit val renderer: Document[EnumType] =
    Document.instance { enumType =>
      import Docs.*
      def toObjectName(_case: String) = _case.toUpperCase.replaceAll("\\W", "_")

      val postfixClassDecl = Doc.text(" extends Product with Serializable")

      val cls =
        Doc
          .text("val value: String")
          .tightBracketBy(
            Doc.text("sealed abstract class ") + Doc.text(enumType.name) + lparens,
            rparens + postfixClassDecl
          )

      val objects =
        Doc.hardLine + Doc.intercalate(
          Doc.hardLine,
          enumType.cases.zip(enumType.descriptions).map { case (k, v) =>
            val obj =
              Doc.text("case object ") + Doc.text(toObjectName(k)) + Doc.text(" extends ") + Doc
                .text(enumType.name) + Doc
                .text(k)
                .tightBracketBy(lparens + quote, quote + rparens)
            val comment = Doc.text("// ") + Doc.text(v)
            comment + Doc.hardLine + obj
          }
        )

      val companionPrefix = Doc.text("object ") + Doc.text(enumType.name) + Doc.space + lbrace

      val values = Doc
        .intercalate(Doc.comma + Doc.line, enumType.cases.map(toObjectName).map(Doc.text))
        .tightBracketBy(
          Doc.text("val values = List") + lparens,
          rparens
        )

      val fromString =
        Doc.text(s"""def fromString(input: String): Either[String, ${enumType.name}]""") +
          Doc.text(" = values.find(_.value == input)") +
          Doc.text(s""".toRight(s"'$$input' was not a valid value for ${enumType.name}")""")

      val decoderInstance =
        TypeClassInstance(
          "decoder",
          ParamType.simple(s"Decoder[${enumType.name}]"),
          Doc.text("Decoder[String].emap(s => fromString(s))"))
      val encoderInstance =
        TypeClassInstance(
          "encoder",
          ParamType.simple(s"Encoder[${enumType.name}]"),
          Doc.text("Encoder[String].contramap(_.value)"))

      val body =
        Doc.intercalate(
          Doc.hardLine,
          List(
            objects,
            values,
            fromString,
            Doc.renderDoc(decoderInstance),
            Doc.renderDoc(encoderInstance)))

      val companion = body.tightBracketBy(companionPrefix, Doc.hardLine + rbrace)

      Doc.intercalate(Doc.hardLine, List(cls, companion)) + Doc.hardLine
    }

}

case class CaseClass(
    name: String,
    parameters: List[Parameter]
) extends GeneratedType {

  def imports = {
    def go(param: ParamType, imports: List[String]): List[String] = param match {
      case ListType(elemType) => go(elemType, imports)
      case ImportedType(fqcn, _) => fqcn :: imports
      case _ => imports
    }
    List("JsonInstances._", "io.circe._", "io.circe.syntax._") ::: parameters
      .flatMap(p => go(p.`type`, Nil))
      .distinct
      .reverse
  }
}

object CaseClass {
  import Docs.*

  implicit val renderer: Document[CaseClass] =
    Document.instance { cc =>
      def render: Doc = {
        val prefix = Doc.text("case class ") + Doc.text(cc.name) + lparens
        val suffix = rparens
        val types = cc.parameters.map(p => Doc.renderDoc(p))
        val body = Doc.intercalate(Doc.comma + Doc.line, types)
        body.tightBracketBy(prefix, suffix)
      }

      def renderCompanion: Doc = {
        val prefix = Doc.text("object ") + Doc.text(cc.name) + Doc.space + lbrace
        val suffix = rbrace
        val instances =
          Doc.intercalate(
            Doc.hardLine,
            List(Doc.renderDoc(encoderInstance), Doc.renderDoc(decoderInstance)))

        instances.tightBracketBy(prefix, suffix)
      }

      def encoderInstance: TypeClassInstance = {
        val obj = Doc
          .intercalate(
            Doc.comma + Doc.line,
            cc.parameters.map(p =>
              Doc.text(p.name).tightBracketBy(quote, quote) + Doc.line + Doc.text(
                ":=") + Doc.line + Doc
                .text("x." + Sanitize(p.name)))
          )
          .tightBracketBy(Doc.text("Json.obj("), rparens)

        TypeClassInstance(
          "encoder",
          ParamType.simple(s"Encoder[${cc.name}]"),
          obj.tightBracketBy(Doc.text("Encoder.instance{ x =>"), rbrace)
        )
      }

      def decoderInstance = {
        val withIndex = cc.parameters.toVector.zipWithIndex
        val cases = Doc.hardLine + Doc.intercalate(
          Doc.hardLine,
          withIndex
            .map { case (p, idx) =>
              Doc.text(s"v$idx") + Doc.line + Doc.text("<-") + Doc.line + Doc.text("c.get[") + Doc
                .text(p.asType) + Doc
                .text("](") + Doc.text(p.name).tightBracketBy(quote, quote) + Doc.text(")")
            }
        )
        val yieldBlock =
          Doc
            .intercalate(
              Doc.comma + Doc.line,
              withIndex.map { case (_, idx) => Doc.text(s"v$idx") })
            .tightBracketBy(
              Doc.hardLine + rbrace + Doc.text(" yield ") + Doc.text(cc.name) + lparens,
              rparens
            )

        val forcomp =
          cases.tightBracketBy(Doc.text("for {"), yieldBlock)

        TypeClassInstance(
          "decoder",
          ParamType.simple(s"Decoder[${cc.name}]"),
          forcomp.tightBracketBy(
            Doc.text("Decoder.instance{ c => "),
            rbrace
          )
        )
      }

      Doc.intercalate(Doc.hardLine, List(render, renderCompanion)) + Doc.hardLine
    }
}

case class Parameter(
    name: String,
    `type`: ParamType,
    description: Option[String],
    required: Boolean
) {
  def asType = if (required) s"${`type`.name}" else s"Option[${`type`.name}]"

  /*override def toString: String = {
    val comment = description.fold("")("// " + _ + "\n")
    if (required)
      s"$comment${Sanitize(name)}: $asType"
    else
      s"$comment${Sanitize(name)}: $asType = None"
  }*/
}

object Parameter {
  implicit val renderer: Document[Parameter] = Document.instance { p =>
    val comment = p.description.fold(Doc.empty)(c => Doc.text("// ") + Doc.text(c) + Doc.hardLine)
    comment + Doc.text(Sanitize(p.name)) + Doc.char(':') + Doc.space + Doc.text(p.asType)
  }
}

object Sanitize {
  def apply(s: String): String = s match {
    case "type" => "`type`"
    case s => s
  }
}
