package discovery

import munit.FunSuite
import io.circe.{Decoder, Json, jawn}

class DecodeErrorTest extends FunSuite {
  test("decode error") {
    val input = jawn
      .parse("""{
                  |  "error": {
                  |    "code": 401,
                  |    "message": "Request had invalid authentication credentials. Expected OAuth 2 access token, login cookie or other valid authentication credential. See https://developers.google.com/identity/sign-in/web/devconsole-project.",
                  |    "errors": [
                  |      {
                  |        "message": "Invalid Credentials",
                  |        "domain": "global",
                  |        "reason": "authError",
                  |        "location": "Authorization",
                  |        "locationType": "header"
                  |      }
                  |    ],
                  |    "status": "UNAUTHENTICATED",
                  |    "details": [
                  |      {
                  |        "@type": "type.googleapis.com/google.rpc.ErrorInfo",
                  |        "reason": "ACCESS_TOKEN_EXPIRED",
                  |        "domain": "googleapis.com",
                  |        "metadata": {
                  |          "method": "google.cloud.bigquery.v2.JobService.InsertJob",
                  |          "service": "bigquery.googleapis.com"
                  |        }
                  |      }
                  |    ]
                  |  }
                  |}
                  |""".stripMargin)
      .getOrElse(Json.obj())

    val decoded = input.as[GoogleError]
    assert(decoded.isRight)
    val right = decoded.right.get
    assertEquals(
      right.message,
      Some(
        "Request had invalid authentication credentials. Expected OAuth 2 access token, login cookie or other valid authentication credential. See https://developers.google.com/identity/sign-in/web/devconsole-project.")
    )
  }
}
