package discovery

import org.http4s.{Method, Uri}
import org.typelevel.paiges.Doc

case class Client(name: String, baseUri: Uri, methods: List[Client.ResolvedInvocation]) {
  def toCode = {
    val definition = Doc.text(s"class ${name}[F: Concurrent](client: Client[F]) ") +
      Docs.blocks(
        List(
          Docs.assigment(
            Doc.text("val baseUri"),
            Docs.interpolate("uri", Doc.text(baseUri.renderString)) + Doc.hardLine)
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
        case "integer" => ParamType.simple("Int")
        case "boolean" => ParamType.simple("Boolean")
        case _ => ParamType.simple("String")
      }
      Parameter(name, typ, Some(param.description), param.required.getOrElse(false))
    }

    val resolveTypes = discovery.schemas.keys.map(typ => typ -> ParamType.simple(typ)).toMap

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
      requestType: Option[ParamType],
      responseType: Option[ParamType]
  ) {
    def toCode = {
      val left = Doc.text(s"def ${name}(") + Doc.hardLine
      val params = template.paramsAsDoc + Doc.hardLine + Docs.rparens
      val inputParam = requestType
        .map(pt =>
          (Doc.text("input:") + Doc.line + Doc.text(pt.name))
            .tightBracketBy(Docs.lparens + Doc.lineOrEmpty, Doc.lineOrEmpty + Docs.rparens))
        .getOrElse(Doc.empty)
      val returnType =
        responseType.map(p => Doc.text(s"Option[${p.name}]")).getOrElse(Doc.text("Status"))

      val request = {
        val left = Doc.text("Request[F](")
        val meth = Docs.assigment(Doc.text("method"), Doc.text(s"Method.${method.name}"))
        val uri = Docs.assigment(Doc.text("uri"), template.toCodeDoc)
        val withBody = if (requestType.isDefined) Doc.text(".withEntity(input)") else Doc.empty
        Doc
          .intercalate(Doc.comma + Doc.lineOrSpace, List(meth, uri))
          .tightBracketBy(
            left,
            Docs.rparens
          ) + withBody
      }
      val clientCall = responseType
        .map(t =>
          Doc
            .text(t.name)
            .tightBracketBy(Doc.text("client.expectOption["), Docs.rbracket))
        .getOrElse(Doc.text("status")) + Docs.lparens

      Docs.assigment(
        Docs.ascribed(left + params + inputParam, returnType),
        Docs.block(request.tightBracketBy(clientCall, Docs.rparens)))
    }
  }
}
