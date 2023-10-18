package discovery

import io.circe.{Codec, Decoder, Encoder}

import scala.concurrent.duration.{DurationLong, FiniteDuration}

class CodegenTest extends munit.FunSuite {

  Codec.from(Decoder[Long], Encoder[Long]).iemap(l => Right(l.millis))(_.toMillis)
  test("case class") {
    val cc = CaseClass(
      "Person",
      List(
        Parameter("name", ParamType.simple("String"), None, true),
        Parameter("age", ParamType.simple("Int"), None, true)))

    val ccAsString = cc.toString
    val expected = """final case class Person(
                     |  name: String,
                     |  age: Int
                     |)
                     |
                     |object Person {
                     |
                     |  implicit val encoder: _root_.io.circe.Encoder[Person] = _root_.io.circe.Encoder.instance{ x =>
                     |      import io.circe.syntax._
                     |
                     |      io.circe.Json.obj(
                     |        "name" -> x.name.asJson,
                     |        "age" -> x.age.asJson
                     |      )
                     |  }
                     |  implicit val decoder: _root_.io.circe.Decoder[Person] = _root_.io.circe.Decoder.instance{ c =>
                     |      for {
                     |        v0 <- c.get[String]("name")
                     |        v1 <- c.get[Int]("age")
                     |      } yield Person(v0,v1)
                     |  }
                     |}
                     |""".stripMargin

    assertEquals(ccAsString, expected)
  }
}
