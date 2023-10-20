package discovery

import org.http4s.{Method, Uri}

case class Client(name: String, baseUri: Uri, methods: List[Client.ResolvedInvocation]) {
  def toCode = {
    val body =
      methods
        .map(_.toCode(baseUri.renderString)())
        .mkString("\n")

    s"""|class $name[F: Concurrent](client: Client[F]) {
        |  $body
        |}
        |""".stripMargin
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
              invocation.parameters.parameters
                .filter(_._2.location == "path")
                .map { case (pName, p) => mkParameter(pName, p) }
                .toList),
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
    def toCode(baseUri: String)(level: Int = 4) = {
      val ident = " " * level
      val bodyIdent = ident + (" " * 2)
      val inputParam = requestType.map(t => s"(input: ${t.name})").getOrElse("")
      val returnType = responseType.map(p => s"Option[${p.name}]").getOrElse("Status")

      val withRequestBody = if (requestType.isDefined) ".withEntity(input)" else ""
      val requestInstance =
        s"Request[F](method = Method.${method}, uri = ${template.toCode(baseUri)})$withRequestBody"
      val clientCall = responseType.map(t => s"expectOption[${t.name}]").getOrElse("status")

      s"""${ident}def $name(
         |${template.paramsAsString}
         |)$inputParam: F[$returnType] = {
         |${bodyIdent}client.$clientCall($requestInstance)
         |}""".stripMargin
    }
  }
}
