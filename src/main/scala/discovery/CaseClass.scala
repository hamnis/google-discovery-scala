package discovery

import org.typelevel.paiges.{Doc, Document}
import org.typelevel.paiges.Document.ops._

sealed trait Type extends Product with Serializable {
  def name: String
  def asDoc: Doc = Doc.text(name)
}

object Type {
  def simple(in: String): Type = Simple(in)
  def importedType(in: String): Type = Imported(in, in.substring(in.lastIndexOf('.') + 1))

  def list(typ: Type): Type = Constructor(Simple("List"), typ)
  def option(typ: Type): Type = Constructor(Simple("Option"), typ)
  def encoder(typ: Type): Type = Constructor(Simple("Encoder"), typ)
  def decoder(typ: Type): Type = Constructor(Simple("Decoder"), typ)

  case class Simple(name: String) extends Type

  case class Constructor(outer: Type, elemType: Type) extends Type {
    override def name: String = s"${outer.name}[${elemType.name}]"
  }

  case class Imported(fqcn: String, name: String) extends Type
}

sealed trait GeneratedType {
  def name: String
  def imports: List[String]
}

object GeneratedType {
  implicit val renderer: Document[GeneratedType] = Document.instance {
    case cc: CaseClass => CaseClass.renderer.document(cc)
    case e: EnumType => EnumType.renderer.document(e)
  }

}

case class TypeClassInstance(name: String, `type`: Type, body: Doc)
object TypeClassInstance {
  implicit val renderer: Document[TypeClassInstance] = Document.instance(tci =>
    Code.assigment(
      Code.ascribed(Doc.text("implicit val ") + Code.term(tci.name), tci.`type`.asDoc),
      tci.body))
}

case class EnumType(name: String, cases: List[String], descriptions: List[String])
    extends GeneratedType {

  override def imports: List[String] = List("io.circe._", "io.circe.syntax._")
}

object EnumType {
  implicit val renderer: Document[EnumType] =
    Document.instance { enumType =>
      import Code.*
      def toObjectName(_case: String) = _case.toUpperCase.replaceAll("\\W", "_")

      val cls = Doc.text(
        s"sealed abstract class ${enumType.name}(val value: String) extends Product with Serializable")

      val objects =
        Doc.hardLine + Doc.intercalate(
          Doc.hardLine,
          enumType.cases.zip(enumType.descriptions).map { case (k, v) =>
            val obj =
              Doc.text(s"case object ${toObjectName(k)} extends ${enumType.name}") + Doc
                .text(k)
                .tightBracketBy(lparens + quote, quote + rparens)
            val comment = Doc.text("// ") + Doc.text(v)
            comment + Doc.hardLine + obj
          }
        )

      val companionPrefix = Doc.text(s"object ${enumType.name} {")

      val values = Doc
        .intercalate(Doc.comma + Doc.line, enumType.cases.map(toObjectName).map(Doc.text))
        .tightBracketBy(
          Doc.text("val values = List") + lparens,
          rparens
        )

      val fromString =
        Code.Def(
          "fromString",
          Nil,
          List(Parameter("input", Type.simple("String"), None, required = true)),
          Some(Type.simple(s"Either[String, ${enumType.name}]")), {
            val msg = Code.interpolate(
              "s",
              Doc.text(s"'$$input' was not a valid value for ${enumType.name}"))
            Code.termSelect(
              Code.termSelect(Code.term("values"), Doc.text("find(_.value == input)")),
              Code.termApply(Doc.text("toRight"), Nil, msg)
            )
          }
        )

      val decoderInstance =
        TypeClassInstance(
          "decoder",
          Type.decoder(Type.simple(enumType.name)),
          Doc.text("Decoder[String].emap(s => fromString(s))"))
      val encoderInstance =
        TypeClassInstance(
          "encoder",
          Type.encoder(Type.simple(enumType.name)),
          Doc.text("Encoder[String].contramap(_.value)"))

      val body =
        Doc.intercalate(
          Doc.hardLine,
          List(objects, values, fromString, decoderInstance.doc, encoderInstance.doc))

      val companion = body.tightBracketBy(companionPrefix, Doc.hardLine + rbrace)

      Doc.intercalate(Doc.hardLine, List(cls, companion)) + Doc.hardLine
    }

}

case class CaseClass(
    name: String,
    parameters: List[Parameter]
) extends GeneratedType {

  def imports = {
    def go(param: Type, imports: List[String]): List[String] = param match {
      case Type.Constructor(outer, elemType) => go(outer, imports) ++ go(elemType, imports)
      case Type.Imported(fqcn, _) => fqcn :: imports
      case _ => imports
    }
    List("JsonInstances._", "io.circe._", "io.circe.syntax._") ::: parameters
      .flatMap(p => go(p.`type`, Nil))
      .distinct
      .reverse
  }
}

object CaseClass {

  implicit val renderer: Document[CaseClass] =
    Document.instance { cc =>
      def render: Doc = {
        val prefix = Doc.text(s"case class ${cc.name}")
        val params = if (cc.parameters.isEmpty) Doc.text("()") else Code.paramsToDoc(cc.parameters)
        prefix + params
      }

      def renderCompanion: Doc = {
        val prefix = Doc.text(s"object ${cc.name} ")
        prefix + Code.blocks(List(encoderInstance.doc, decoderInstance.doc))
      }

      def encoderInstance: TypeClassInstance = {
        val obj = Doc
          .intercalate(
            Doc.comma + Doc.lineOrSpace,
            cc.parameters.map(p =>
              Code.literal(Doc.text(p.name)) + Doc.lineOrSpace + Doc.text(
                ":=") + Doc.lineOrSpace + Code
                .termSelect(Doc.text("x"), Code.term(p.name)))
          )
          .tightBracketBy(Doc.text("Json.obj("), Code.rparens)

        TypeClassInstance(
          "encoder",
          Type.encoder(Type.simple(cc.name)),
          obj.tightBracketBy(Doc.text("Encoder.instance{ x =>"), Code.rbrace)
        )
      }

      def decoderInstance = {
        val withIndex = cc.parameters.zipWithIndex

        val forcomp =
          Code.forComprehension(
            withIndex.map { case (p, idx) =>
              Doc.text(s"v$idx") -> Code.termApply(
                Doc.text("c.get"),
                List(p.actualType),
                Code
                  .literal(Doc.text(p.name)))
            },
            Code.termApply(
              Doc.text(cc.name),
              Nil,
              Doc
                .intercalate(
                  Doc.comma + Doc.lineOrSpace,
                  withIndex.map { case (_, idx) => Doc.text(s"v$idx") }))
          )

        TypeClassInstance(
          "decoder",
          Type.decoder(Type.simple(cc.name)),
          forcomp.tightBracketBy(
            Doc.text("Decoder.instance{ c => "),
            Doc.hardLine + Code.rbrace
          )
        )
      }

      Doc.intercalate(Doc.hardLine, List(render, renderCompanion)) + Doc.hardLine
    }
}

case class Parameter(
    name: String,
    `type`: Type,
    description: Option[String],
    required: Boolean
) {
  def actualType = if (required) `type` else Type.option(`type`)
  def asDoc = actualType.asDoc
}

object Parameter {
  implicit val renderer: Document[Parameter] = Document.instance { p =>
    val comment = p.description.fold(Doc.empty)(c => Doc.text("// ") + Doc.text(c) + Doc.hardLine)
    comment + Code.ascribed(Code.term(p.name), p.asDoc)
  }
}
