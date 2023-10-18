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
                     |  implicit lazy val codec: _root_.io.circe.Codec[Person] = _root_.io.circe.Codec.forProduct2("name", "age")(Person.apply)(x => (x.name, x.age))
                     |}
                     |""".stripMargin

    assertEquals(ccAsString, expected)
  }
}
