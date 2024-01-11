package discovery

import org.http4s.{Method, Uri}
import org.typelevel.paiges.Doc
import org.typelevel.paiges.Document.ops._

case class Client(name: String, baseUri: Uri, methods: List[Client.ResolvedInvocation]) {
  def imports =
    List(
      "cats.effect.Concurrent",
      "org.http4s._",
      "org.http4s.implicits._",
      "org.http4s.circe._",
      "org.http4s.client.Client",
      "io.circe.{Encoder, Decoder}"
    ) ++ methods.flatMap(m => m.findTypes.flatMap(Type.findImports(_, Nil)))

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

    val definitions = methods.flatMap(_.queryParams.caseClass).distinct

    val companion =
      if (definitions.isEmpty) Doc.empty
      else
        Doc.hardLine + Doc.text(s"object ${name} ") + Code.blocks(
          definitions.map(cc => CaseClass.renderClass(cc))
        )

    (definition + companion).render(80)
  }
}

object Client {
  def clientsFrom(discovery: Discovery) = {
    val resources = discovery.resources.getOrElse(Resources(Map.empty)).resources
    def mkParameter(name: String, param: HttpParameter) = {
      val typ = param.`type` match {
        case "integer" => Type("Int")
        case "boolean" => Type("Boolean")
        case _ => Type("String")
      }
      Parameter(
        name,
        typ,
        Some(param.description),
        param.required.getOrElse(false)
      )
    }

    val resolveTypes = discovery.schemas.keys.map(typ => typ -> Type.apply(typ)).toMap

    resources.flatMap { case (resourceName, resource) =>
      resource.methods.values.map { invocations =>
        val resourceTypeName = resourceName.capitalize
        val resolved = invocations.methods.map { case (method, invocation) =>
          ResolvedInvocation(
            resourceTypeName,
            method,
            Method.fromString(invocation.httpMethod).right.get,
            Template(
              invocation.path,
              invocation.parameters.order.flatMap(p =>
                invocation.parameters.parameters.get(p).map(mkParameter(p, _)))
            ),
            QueryParams(
              invocation.httpMethod.toLowerCase.capitalize,
              invocation.parameters.parameters
                .collect {
                  case (k, v) if v.`type` == "query" =>
                    mkParameter(k, v.copy(required = None)).copy(default = Some(Doc.text("None")))
                }
                .toList
                .sortBy(_.name)
            ),
            invocation.request.flatMap(s => s.$ref).flatMap(resolveTypes.get),
            invocation.response.flatMap(s => s.$ref).flatMap(resolveTypes.get)
          )
        }.toList

        Client(
          resourceTypeName + "Client",
          discovery.baseUrl.withPath(discovery.baseUrl.path.dropEndsWithSlash),
          resolved)
      }
    }.toList
  }

  case class ResolvedInvocation(
      resourceTypeName: String,
      name: String,
      method: Method,
      template: Template,
      queryParams: QueryParams,
      requestType: Option[Type],
      responseType: Option[Type]
  ) {
    def findTypes =
      List(requestType.toList, responseType.toList, template.params.map(_.`type`)).flatten

    def toCode = {
      val assigned = {
        def paramsAsDoc(params: List[Parameter]) = Doc
          .intercalate(Doc.comma + Doc.line, params.map(p => p.doc))

        val left = Doc.text(s"def ${name}(") + Doc.hardLine
        val qp =
          if (queryParams.isEmpty) {
            val paramTypeName = Type(resourceTypeName + "Client." + queryParams.typeName)
            template.params ::: List(
              Parameter(
                "query",
                paramTypeName,
                None,
                required = true,
                default =
                  if (queryParams.isEmpty) None
                  else
                    Some(paramTypeName.asDoc + Doc.text("()"))))
          } else template.params
        val params = paramsAsDoc(qp) + Doc.hardLine + Code.rparens
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
          .constructor(Type("F"), responseType.map(Type.option).getOrElse(Type.apply("Status")))
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
          t.asDoc
            .tightBracketBy(Doc.text("client.expectOption["), Code.rbracket))
        .getOrElse(Doc.text("client.status")) + Code.lparens

      Code.assigment(
        Code.ascribed(assigned, returnType),
        Code.block(request.tightBracketBy(clientCall, Code.rparens)))
    }
  }
}
