package discovery

import org.typelevel.paiges.Document.ops._

class CodegenTest extends munit.FunSuite {
  test("case class") {
    val cc = CaseClass(
      "Person",
      List(
        Parameter(
          "object",
          Type("String"),
          Some("This is a comment and it is awesome\nWe generate newlines\nmore newlines"),
          true),
        Parameter("age", Type("Int"), None, true)
      )
    )

    val ccAsString = CaseClass.renderer.document(cc).render(80)
    val expected = """final case class Person(
                     |  /**
                     |  * This is a comment and it is awesome
                     |  * We generate newlines
                     |  * more newlines
                     |  */
                     |  `object`: String, age: Int)
                     |object Person {
                     |  implicit val encoder: Encoder[Person] = Encoder.instance{ x =>
                     |    Json.obj("object" := x.`object`, "age" := x.age)
                     |  }
                     |  implicit val decoder: Decoder[Person] = Decoder.instance{ c => for {
                     |      v0 <- c.get[String]("object")
                     |      v1 <- c.get[Int]("age")
                     |    } yield Person(v0, v1)
                     |  }
                     |}
                     |""".stripMargin

    assertEquals(ccAsString, expected)
  }

  test("enum") {
    val enumType = EnumType("Kind", List("one", "two"), List("Lots of ones", "Even more twos"))

    val rendered = enumType.doc.render(80)

    val expected = """sealed abstract class Kind(val value: String) extends Product with Serializable
                     |object Kind {
                     |  /**
                     |  * Lots of ones
                     |  */
                     |  case object ONE extends Kind("one")
                     |  
                     |  /**
                     |  * Even more twos
                     |  */
                     |  case object TWO extends Kind("two")
                     |  val values = List(ONE, TWO)
                     |  def fromString(input: String): Either[String, Kind] = values.find(_.value == input).toRight(s"'$input' was not a valid value for Kind")
                     |  implicit val decoder: Decoder[Kind] = Decoder[String].emap(s => fromString(s))
                     |  implicit val encoder: Encoder[Kind] = Encoder[String].contramap(_.value)
                     |}
                     |""".stripMargin
    assertEquals(rendered, expected)
  }
}
