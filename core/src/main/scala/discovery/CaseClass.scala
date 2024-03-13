package discovery

import org.typelevel.paiges.{Doc, Document}
import org.typelevel.paiges.Document.ops._

sealed trait Type extends Product with Serializable {
  def asDoc: Doc
  def asString = asDoc.render(80)
}

object Type {
  def apply(in: String): Type = Simple(in)
  def importedType(in: String): Type = Imported(in, in.substring(in.lastIndexOf('.') + 1))
  def constructor(outer: Type, inner: Type*): Type = Constructor(outer, inner.toList)

  def list(typ: Type): Type = constructor(apply("List"), typ)
  def option(typ: Type): Type = constructor(apply("Option"), typ)
  def map(left: Type, right: Type): Type = constructor(apply("Map"), left, right)
  def encoder(typ: Type): Type = constructor(apply("Encoder"), typ)
  def decoder(typ: Type): Type = constructor(apply("Decoder"), typ)

  case class Simple(name: String) extends Type {
    val asDoc = Doc.text(name)
  }

  case class Constructor(outer: Type, elemTypes: List[Type]) extends Type {

    override def asDoc: Doc = Doc
      .intercalate(Doc.comma + Doc.lineOrSpace, elemTypes.map(_.asDoc))
      .tightBracketBy(outer.asDoc + Code.lbracket, Code.rbracket)
  }

  case class Imported(qualified: String, name: String) extends Type {
    def asDoc = Doc.text(name)
  }

  def findImports(param: Type, imports: List[String]): List[String] = param match {
    case Type.Constructor(outer, elemTypes) =>
      findImports(outer, elemTypes.flatMap(findImports(_, imports)))
    case Type.Imported(fqcn, _) => fqcn :: imports
    case _ => imports
  }
}

sealed trait GeneratedType {
  def name: String
  def imports: List[String]
}

object GeneratedType {
  implicit val renderer: Document[GeneratedType] = Document.instance {
    case cc: CaseClass => CaseClass.renderer.document(cc)
    case e: EnumType => EnumType.renderer.document(e)
    case w: JsonObjectWrapper => JsonObjectWrapper.renderer.document(w)
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

  override def imports: List[String] = List("io.circe._")
}

object EnumType {
  implicit val renderer: Document[EnumType] =
    Document.instance { enumType =>
      def toObjectName(_case: String) = _case.toUpperCase.replaceAll("\\W", "_")

      val cls = Doc.text(
        s"sealed abstract class ${enumType.name}(val value: String) extends Product with Serializable")

      val objects =
        Doc.intercalate(
          Doc.hardLine,
          enumType.cases.zip(enumType.descriptions).map { case (k, v) =>
            val obj =
              Doc.text(s"case object ${toObjectName(k)} extends ${enumType.name}") + Doc
                .text(k)
                .tightBracketBy(Code.lparens + Code.quote, Code.quote + Code.rparens, 0)
            val comment = Code.blockComment(v)
            comment + obj
          }
        )

      val companionPrefix = Doc.text(s"object ${enumType.name} {")

      val values = Doc
        .intercalate(Doc.comma + Doc.line, enumType.cases.map(toObjectName).map(Doc.text))
        .tightBracketBy(
          Doc.text("val values = List") + Code.lparens,
          Code.rparens
        )

      val enumAsType = Type(enumType.name)
      val fromString =
        Code.Def(
          "fromString",
          Nil,
          List(Parameter("input", Type("String"), None, required = true)),
          Some(Type.constructor(Type("Either"), Type("String"), enumAsType)), {
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
          Type.decoder(enumAsType),
          Doc.text("Decoder[String].emap(s => fromString(s))"))
      val encoderInstance =
        TypeClassInstance(
          "encoder",
          Type.encoder(enumAsType),
          Doc.text("Encoder[String].contramap(_.value)"))

      val body =
        Doc.intercalate(
          Doc.hardLine,
          List(objects, values, fromString, decoderInstance.doc, encoderInstance.doc))

      val companion = body.tightBracketBy(companionPrefix, Doc.hardLine + Code.rbrace)

      Doc.intercalate(Doc.hardLine, List(cls, companion)) + Doc.hardLine
    }

}

case class JsonObjectWrapper(name: String) extends GeneratedType {
  override def imports: List[String] = List(
    "io.circe._"
  )
}

object JsonObjectWrapper {
  implicit val renderer: Document[JsonObjectWrapper] = (a: JsonObjectWrapper) => {
    val cc = Doc.text(s"final case class ${a.name}(value: JsonObject)")
    val companion = {
      val start = Doc.text(s"object ${a.name} ")
      val encoder = TypeClassInstance(
        "encoder",
        Type.encoder(Type(a.name)),
        Doc.text("Encoder[JsonObject].contramap(_.value)"))
      val decoder = TypeClassInstance(
        "decoder",
        Type.decoder(Type(a.name)),
        Doc.text("Decoder[JsonObject].map(apply)"))
      start + Code.blocks(List(encoder.doc, decoder.doc))
    }

    cc + Doc.hardLine + companion
  }
}

case class CaseClass(
    name: String,
    parameters: List[Parameter]
) extends GeneratedType {
  val asType: Type = Type(name)

  def jsonInstancesImport =
    if (parameters.exists { p =>
        val string = p.`type`.asString
        string.contains("FiniteDuration") || string.contains("ByteVector")
      }) List("JsonInstances._")
    else Nil

  def imports =
    jsonInstancesImport ::: List("io.circe._", "io.circe.syntax._") ::: parameters
      .flatMap(p => Type.findImports(p.`type`, Nil))
      .distinct
      .reverse
}

object CaseClass {

  def renderClass(cc: CaseClass) = {
    val prefix = Doc.text(s"final case class ${cc.name}")
    val params = if (cc.parameters.isEmpty) Doc.text("()") else Code.paramsToDoc(cc.parameters)
    prefix + params
  }

  implicit val renderer: Document[CaseClass] =
    Document.instance { cc =>
      val render = renderClass(cc)

      def renderCompanion: Doc = {
        val prefix = Doc.text(s"object ${cc.name} ")
        prefix + Code.blocks(List(encoderInstance.doc, decoderInstance.doc))
      }

      def encoderInstance: TypeClassInstance = {
        val obj = Doc
          .intercalate(
            Doc.comma + Doc.lineOrSpace,
            cc.parameters.map(p =>
              p.literal + Doc.lineOrSpace + Doc.text(":=") + Doc.lineOrSpace + Code
                .termSelect(Doc.text("x"), p.term))
          )
          .tightBracketBy(Doc.text("Json.obj("), Code.rparens)

        TypeClassInstance(
          "encoder",
          Type.encoder(cc.asType),
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
          Type.decoder(cc.asType),
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
    required: Boolean,
    default: Option[Doc] = None
) {
  def literal = Code.literal(Doc.text(name))
  def term = Code.term(name)
  def actualType = if (required) `type` else Type.option(`type`)
  def asDoc = actualType.asDoc
}

object Parameter {
  implicit val renderer: Document[Parameter] = Document.instance { p =>
    val comment = p.description.fold(Doc.empty)(Code.blockComment)
    val defaultOrEmpty = p.default
      .map(d => Doc.char('=').tightBracketBy(Doc.lineOrSpace, Doc.lineOrSpace) + d)
      .getOrElse(Doc.empty)
    comment + Code.ascribed(p.term, p.asDoc) + defaultOrEmpty
  }
}
