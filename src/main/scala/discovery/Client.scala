package discovery

import org.http4s.{Method, Uri}
import org.typelevel.paiges.Doc

case class Client(name: String, baseUri: Uri, methods: List[Client.ResolvedInvocation]) {
  def imports =
    List(
      "cats.effect.Concurrent",
      "org.http4s._",
      "org.http4s.implicits._",
      "org.http4s.circe._",
      "org.http4s.client.Client",
      "io.circe.{Encoder, Decoder}"
    )

  def toCode = {
    val definition = Doc.text(s"class ${name}[F[_]: Concurrent](client: Client[F]) ") +
      Code.blocks(
        List(
          Code.assigment(
            Code.ascribed(
              Doc.text("private implicit def entityEncoder[A: Encoder]"),
              Doc.text("EntityEncoder[F, A]")),
            Doc.text("jsonEncoderOf[F, A]")
          ),
          Code.assigment(
            Code.ascribed(
              Doc.text("private implicit def entityDecoder[A: Decoder]"),
              Doc.text("EntityDecoder[F, A]")),
            Doc.text("jsonOf[F, A]")
          ),
          Code.assigment(
            Doc.text("val baseUri"),
            Code.interpolate("uri", Doc.text(baseUri.renderString)) + Doc.hardLine)
        ) ++ methods.map(_.toCode + Doc.hardLine)
      )

    definition.render(80)
  }
}

object Client {
  def clientsFrom(discovery: Discovery) = {
    val resources = discovery.resources.getOrElse(Resources(Map.empty)).resources
    def mkParameter(name: String, param: HttpParameter) = {
      val typ = param.`type` match {
        case "integer" => Type.simple("Int")
        case "boolean" => Type.simple("Boolean")
        case _ => Type.simple("String")
      }
      Parameter(name, typ, Some(param.description), param.required.getOrElse(false))
    }

    val resolveTypes = discovery.schemas.keys.map(typ => typ -> Type.simple(typ)).toMap

    resources.flatMap { case (resourceName, resource) =>
      resource.methods.map { case (name, invocations: Invocations) =>
        val resolved = invocations.methods.map { case (method, invocation) =>
          ResolvedInvocation(
            method,
            Method.fromString(invocation.httpMethod).right.get,
            Template(
              invocation.path,
              invocation.parameters.order.flatMap(p =>
                invocation.parameters.parameters.get(p).map(mkParameter(p, _)))
            ),
            invocation.request.flatMap(s => s.$ref).flatMap(resolveTypes.get),
            invocation.response.flatMap(s => s.$ref).flatMap(resolveTypes.get)
          )
        }.toList

        Client(
          resourceName.capitalize,
          discovery.baseUrl.withPath(discovery.baseUrl.path.dropEndsWithSlash),
          resolved)
      }
    }.toList
  }

  case class ResolvedInvocation(
      name: String,
      method: Method,
      template: Template,
      requestType: Option[Type],
      responseType: Option[Type]
  ) {
    def toCode = {
      val assigned = {
        val left = Doc.text(s"def ${name}(") + Doc.hardLine
        val params = template.paramsAsDoc + Doc.hardLine + Code.rparens
        val inputParam = requestType
          .map(pt =>
            Code
              .ascribed(Doc.text("input"), pt.asDoc)
              .tightBracketBy(Code.lparens + Doc.lineOrEmpty, Doc.lineOrEmpty + Code.rparens))
          .getOrElse(Doc.empty)

        left + params + inputParam
      }

      val returnType =
        Type
          .Constructor(
            Type.simple("F"),
            responseType.map(Type.option).getOrElse(Type.simple("Status")))
          .asDoc

      val request = {
        val withBody = if (requestType.isDefined) Doc.text(".withEntity(input)") else Doc.empty
        Doc
          .intercalate(
            Doc.comma + Doc.lineOrSpace,
            List(
              Code.assigment(Doc.text("method"), Doc.text(s"Method.${method.name}")),
              Code.assigment(Doc.text("uri"), template.toCodeDoc))
          )
          .tightBracketBy(
            Doc.text("Request[F]("),
            Code.rparens
          ) + withBody
      }
      val clientCall = responseType
        .map(t =>
          Doc
            .text(t.name)
            .tightBracketBy(Doc.text("client.expectOption["), Code.rbracket))
        .getOrElse(Doc.text("client.status")) + Code.lparens

      Code.assigment(
        Code.ascribed(assigned, returnType),
        Code.block(request.tightBracketBy(clientCall, Code.rparens)))
    }
  }
}
