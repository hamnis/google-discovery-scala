package discovery

import org.http4s.{Method, Uri}
import org.typelevel.paiges.Doc
import org.typelevel.paiges.Document.ops._
import scala.collection.compat._

case class ClientCodegen(
    name: String,
    baseUri: Uri,
    methods: List[ClientCodegen.ResolvedApiMethod]) {
  def imports =
    List(
      "cats.effect.Concurrent",
      "org.http4s._",
      "org.http4s.implicits._",
      "org.http4s.client.Client"
    ) ++ methods.flatMap(m => m.findTypes.flatMap(Type.findImports(_, Nil)))

  def toCode = {
    val definition =
      Doc.text(
        s"class ${name}[F[_]: Concurrent](client: Client[F]) extends AbstractClient[F](client) ") +
        Code.blocks(
          List(
            List(
              Code.assigment(
                Doc.text("val baseUri"),
                Code.interpolate("uri", Doc.text(baseUri.renderString)) + Doc.hardLine)
            )).flatten ++ methods.map(_.toCode + Doc.hardLine)
        )

    val definitions = distinctBy(methods.flatMap(_.queryParams.caseClass))(_.name)

    val companion =
      if (definitions.isEmpty) Doc.empty
      else
        Doc.hardLine + Doc.text(s"object ${name} ") + Code.blocks(
          definitions.map(cc => CaseClass.renderClass(cc) + Doc.hardLine)
        )

    (definition + companion).render(80)
  }

  private def distinctBy[A, B](list: List[A])(f: A => B) =
    if (list.lengthCompare(1) <= 0) list
    else {
      val builder = List.newBuilder[A]
      val seen = collection.mutable.HashSet.empty[B]
      val it = list.iterator
      var different = false
      while (it.hasNext) {
        val next = it.next()
        if (seen.add(f(next))) builder += next else different = true
      }
      if (different) builder.result() else list
    }
}

object ClientCodegen {

  def clientsFrom(discovery: Discovery) = {
    val resolveTypes = discovery.schemas.keys.map(typ => typ -> Type.apply(typ)).toMap

    def fromResources(parentName: Option[String], _resources: Resources): List[ClientCodegen] = {
      val resources = _resources.resources

      resources.flatMap { case (resourceName, resource) =>
        val resourceTypeName = parentName.getOrElse("") + resourceName.capitalize
        val resolved = resolveApiMethods(resourceTypeName, resource, resolveTypes)

        val recursive = fromResources(Some(resourceTypeName), resource.resources)
        val ourClients = if (resolved.nonEmpty) {
          List(
            ClientCodegen(
              resourceTypeName + "Client",
              discovery.baseUrl.withPath(discovery.baseUrl.path.dropEndsWithSlash),
              resolved)
          )
        } else Nil

        ourClients ::: recursive
      }.toList
    }

    fromResources(None, discovery.resources)
  }

  private def resolveApiMethods(
      resourceTypeName: String,
      resource: Resource,
      types: Map[String, Type]) = {
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

    resource.methods.map { case (methodName, apiMethod) =>
      ResolvedApiMethod(
        resourceTypeName,
        methodName,
        Method.fromString(apiMethod.httpMethod).toOption.get,
        Template(
          apiMethod.path,
          apiMethod.parameters.order.flatMap(p =>
            apiMethod.parameters.parameters
              .get(p)
              .filter(_.location == "path")
              .map(mkParameter(p, _)))
        ),
        QueryParams(
          methodName.capitalize,
          apiMethod.parameters.parameters
            .collect {
              case (k, v) if v.location == "query" =>
                mkParameter(k, v.copy(required = None)).copy(default = Some(Doc.text("None")))
            }
            .toList
            .sortBy(_.name)
        ),
        apiMethod.request.flatMap(s => s.$ref).flatMap(types.get),
        apiMethod.response.flatMap(s => s.$ref).flatMap(types.get)
      )
    }.toList
  }

  case class ResolvedApiMethod(
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
          if (queryParams.nonEmpty) {
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
          .constructor(Type("F"), responseType.getOrElse(Type.apply("Status")))
          .asDoc

      val request = {
        val withBody = if (requestType.isDefined) Doc.text("(input)") else Doc.empty
        Doc
          .intercalate(
            Doc.comma + Doc.lineOrSpace,
            List(
              Code.assigment(Doc.text("method"), Doc.text(s"Method.${method.name}")),
              Code.assigment(Doc.text("uri"), template.toCodeDoc(queryParams)))
          )
          .tightBracketBy(
            if (requestType.isDefined) Doc.text("requestWithBody(") else Doc.text("request("),
            Code.rparens
          ) + withBody
      }
      val clientCall = responseType
        .map(t =>
          t.asDoc
            .tightBracketBy(Doc.text("expectJson["), Code.rbracket))
        .getOrElse(Doc.text("client.status")) + Code.lparens

      Code.assigment(
        Code.ascribed(assigned, returnType),
        Code.block(request.tightBracketBy(clientCall, Code.rparens)))
    }
  }
}
