package discovery

import io.circe.Decoder

case class GoogleError(
    code: Option[Int],
    message: Option[String],
    errors: List[GoogleError.ErrorInfo],
    details: List[GoogleError.Details]
) extends Exception(message.getOrElse(""))

object GoogleError {
  final case class ErrorInfo(
      domain: Option[String],
      reason: Option[String],
      message: Option[String],
      location: Option[String],
      locationType: Option[String]
  )

  object ErrorInfo {
    implicit val decoder: Decoder[ErrorInfo] =
      Decoder.forProduct5("domain", "reason", "message", "location", "locationType")(apply)
  }

  final case class Details(
      `type`: Option[String],
      reason: Option[String],
      parameterViolations: List[ParameterViolation])
  object Details {
    implicit val decoder: Decoder[Details] = Decoder.instance(c =>
      for {
        t <- c.get[Option[String]]("@type")
        r <- c.get[Option[String]]("reason")
        v <- c.get[Option[List[ParameterViolation]]]("parameterViolations")
      } yield Details(t, r, v.getOrElse(Nil)))
  }

  final case class ParameterViolation(parameter: Option[String], description: Option[String])

  object ParameterViolation {
    implicit val decoder: Decoder[ParameterViolation] =
      Decoder.forProduct2("parameter", "description")(apply)
  }

  implicit val decoder: Decoder[GoogleError] = Decoder
    .instance(c =>
      for {
        code <- c.get[Option[Int]]("code")
        message <- c.get[Option[String]]("message")
        errors <- c.get[Option[List[ErrorInfo]]]("errors")
        details <- c.get[Option[List[Details]]]("details")
      } yield GoogleError(code, message, errors.getOrElse(Nil), details.getOrElse(Nil)))
    .at("error")
}
