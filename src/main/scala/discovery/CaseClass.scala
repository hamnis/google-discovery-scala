package discovery

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

case class EnumType(name: String, cases: List[String]) extends GeneratedType {

  override def imports: List[String] = Nil

  override def toString = {
    def toObjectName(_case: String) = _case.toUpperCase.replaceAll("\\W", "_")
    def objects = cases
      .map(c => s"""  case object ${toObjectName(c)} extends $name("$c")""")
      .mkString("\n")
    val values = cases.map(toObjectName).mkString("List(", ", ", ")")

    s"""sealed abstract class $name(val value: String) extends Product with Serializable
       |object $name {
       |
       |$objects
       |
       |  val values = $values
       |
       |  def fromString(input: String): Either[String, $name] = values.find(_.value == input).toRight(s"'$$input' was not a valid value for $name")
       |
       |  implicit val decoder: _root_.io.circe.Decoder[$name] = _root_.io.circe.Decoder[String].emap(s => fromString(s))
       |  implicit val encoder: _root_.io.circe.Encoder[$name] = _root_.io.circe.Encoder[String].contramap(_.value)
       |}
       |""".stripMargin
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
    ("JsonInstances._" :: (parameters
      .flatMap(p => go(p.`type`, Nil))
      .distinct
      .reverse))
      .map("import " + _)
  }
  private def lit(s: String) = "\"" + s + "\""

  def encoderInstance = {
    val asVector = parameters.toVector
    val instance =
      asVector.map(p => s"${lit(p.name)} -> x.${Sanitize(p.name)}.asJson").mkString(",\n        ")

    s"""Encoder.instance{ x =>
       |      import io.circe.syntax._
       |
       |      io.circe.Json.obj(
       |        $instance
       |      )
       |  }""".stripMargin
  }

  def decoderInstance = {
    val asVector = parameters.toVector
    val cases = asVector.zipWithIndex
      .map { case (p, idx) => s"""v$idx <- c.get[${p.asType}](${lit(p.name)})""" }
      .mkString("\n        ")
    val fields = asVector.zipWithIndex.map { case (_, idx) => s"v$idx" }.mkString(",")

    s"""|Decoder.instance{ c =>
        |      for {
        |        $cases
        |      } yield $name($fields)
        |  }""".stripMargin
  }

  override def toString: String = {
    val name = Sanitize(this.name)
    s"""|final case class $name(
        |${parameters.map("  " + _).mkString(",\n")}
        |)
        |
        |object $name {
        |
        |  implicit val encoder: _root_.io.circe.Encoder[$name] = _root_.io.circe.$encoderInstance
        |  implicit val decoder: _root_.io.circe.Decoder[$name] = _root_.io.circe.$decoderInstance
        |}
        |""".stripMargin
  }
}

case class Parameter(
    name: String,
    `type`: ParamType,
    description: Option[String],
    required: Boolean
) {
  def asType = if (required) s"${`type`.name}" else s"Option[${`type`.name}]"

  override def toString: String = {
    val comment = description.fold("")("// " + _ + "\n")
    if (required)
      s"$comment${Sanitize(name)}: $asType"
    else
      s"$comment${Sanitize(name)}: $asType = None"
  }
}

object Sanitize {
  def apply(s: String): String = s match {
    case "type" => "`type`"
    case s => s
  }
}
