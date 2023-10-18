package discovery

sealed trait ParamType extends Product with Serializable {
  def name: String
}

object ParamType {
  def simple(in: String): ParamType = SimpleType(in)
  def importedType(in: String): ParamType = ImportedType(in, in.substring(in.lastIndexOf('.') + 1))
  def enumType(name: String, cases: List[String]): ParamType =
    EnumType(name, name.substring(name.lastIndexOf('.') + 1), cases)

  def list(typ: ParamType): ParamType = ListType(typ)
}

case class SimpleType(name: String) extends ParamType
case class ListType(elemType: ParamType) extends ParamType {
  override def name: String = s"List[${elemType.name}]"
}
case class ImportedType(fqcn: String, name: String) extends ParamType

case class EnumType(name: String, typeName: String, cases: List[String]) extends ParamType {
  override def toString = {
    def toObjectName(_case: String) = _case.toUpperCase.replaceAll("\\W", "_")
    def objects = cases
      .map(c => s"""  case object ${toObjectName(c)} extends $typeName("$c")""")
      .mkString("\n")
    val values = cases.map(toObjectName).mkString("List(", ", ", ")")

    s"""sealed abstract class $typeName(val value: String)
       |object $typeName {
       |
       |$objects
       |
       |  val values = $values
       |
       |  def fromString(input: String): Either[String, $typeName] = values.find(_.value == input).toRight(s"'$$input' was not a valid value for $typeName")
       |
       |  implicit val decoder: _root_.io.circe.Decoder[$typeName] = _root_.io.circe.Decoder[String].emap(s => fromString(s))
       |  implicit val encoder: _root_.io.circe.Encoder[$typeName] = _root_.io.circe.Encoder[String].contramap(_.value)
       |}
       |""".stripMargin
  }
}

case class CaseClass(
    name: String,
    parameters: List[Parameter]
) {
  def imports = {
    def go(param: ParamType, imports: List[String]): List[String] = param match {
      case ListType(elemType) => go(elemType, imports)
      case ImportedType(fqcn, _) => fqcn :: imports
      case _ => imports
    }
    parameters
      .flatMap(p => go(p.`type`, Nil))
      .distinct
      .reverse
      .map("import " + _)
  }

  private def enums = {
    def go(param: ParamType, enums: List[EnumType]): List[EnumType] = param match {
      case ListType(elemType) => go(elemType, enums)
      case e: EnumType => e :: enums
      case _ => enums
    }
    parameters
      .flatMap(p => go(p.`type`, Nil))
      .distinct
      .reverse
      .map(_.toString)
      .mkString("\n")
  }

  override def toString: String = {
    val name = Sanitize(this.name)
    s"""|final case class $name(
        |${parameters.map("  " + _).mkString(",\n")}
        |)
        |
        |object $name {
        |$enums
        |  implicit lazy val codec: _root_.io.circe.Codec[$name] = _root_.io.circe.generic.semiauto.deriveCodec[$name]
        |}
        |""".stripMargin
  }
}

case class Parameter(
    name: String,
    `type`: ParamType,
    required: Boolean
) {
  override def toString: String =
    if (required)
      s"${Sanitize(name)}: ${`type`.name}"
    else
      s"${Sanitize(name)}: Option[${`type`.name}] = None"
}

object Sanitize {
  def apply(s: String): String = s match {
    case "type" => "`type`"
    case s => s
  }
}
