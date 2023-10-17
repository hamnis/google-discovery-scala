package discovery

import io.circe.{Codec, Decoder, Encoder}

import scala.concurrent.duration.{DurationLong, FiniteDuration}

class CodegenTest extends munit.FunSuite {

  Codec.from(Decoder[Long], Encoder[Long]).iemap(l => Right(l.millis))(_.toMillis)
  test("case class") {
    val cc = CaseClass(
      "Person",
      List(
        Parameter("name", ParamType.simple("String"), true),
        Parameter("age", ParamType.simple("Int"), true)))

    val ccAsString = cc.toString
    val expected = """final case class Person(
                     |  name: String,
                     |  age: Int
                     |)
                     |
                     |object Person {
                     |
                     |  implicit lazy val codec: _root_.io.circe.Codec[Person] = _root_.io.circe.generic.semiauto.deriveCodec[Person]
                     |}
                     |""".stripMargin

    assertEquals(ccAsString, expected)
  }

  test("case class with enum") {
    val cc = CaseClass(
      "Thingy",
      List(
        Parameter("name", ParamType.simple("String"), true),
        Parameter("kind", ParamType.enumType("Thingy.ThingyKind", List("one", "two")), true)))

    val ccAsString = cc.toString
    val expected = """final case class Thingy(
                     |  name: String,
                     |  kind: Thingy.ThingyKind
                     |)
                     |
                     |object Thingy {
                     |sealed abstract class ThingyKind(override val entryName: String) extends EnumEntry
                     |object ThingyKind extends Enum[ThingyKind] with CirceEnum[ThingyKind] {
                     |  lazy val values = findValues
                     |  case object ONE extends ThingyKind("one")
                     |  case object TWO extends ThingyKind("two")
                     |
                     |}
                     |
                     |  implicit lazy val codec: _root_.io.circe.Codec[Thingy] = _root_.io.circe.generic.semiauto.deriveCodec[Thingy]
                     |}
                     |""".stripMargin

    assertEquals(ccAsString, expected)
    val errors = compileErrors(
      """final case class Thingy(
          name: String,
          kind: Thingy.ThingyKind
        )

        object Thingy {
        sealed abstract class ThingyKind(override val entryName: String) extends enumeratum.EnumEntry
        object ThingyKind extends enumeratum.Enum[ThingyKind] with enumeratum.CirceEnum[ThingyKind] {
          val values = findValues
          case object one extends ThingyKind("one")
          case object two extends ThingyKind("two")

        }

          implicit lazy val codec: _root_.io.circe.Codec[Thingy] = _root_.io.circe.generic.semiauto.deriveCodec[Thingy]
        }
        """)
    assertEquals(errors, "")
  }
}
