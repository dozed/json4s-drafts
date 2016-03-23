package oauth

import org.json4s._
import org.json4s.jackson.{parseJson, parseJsonOpt}
import org.json4s.ext.scalaz.JsonScalaz._

import scala.collection.immutable.BitSet
import scala.concurrent.Future
import scala.util.Try
import scalaz.concurrent.Task
import TaskC.toTask

import dispatch._, Defaults._
import rl.UrlCodingUtils

import scalaz._, Scalaz._

trait OAuthTypes {

  sealed trait ErrorCode
  object ErrorCode {
    case class TokenResponseError(error: String, errorDescription: String) extends ErrorCode
    case class InvalidResponse(message: String) extends ErrorCode
    case class AuthorizationError(code: String, message: String) extends ErrorCode
    case object ParseError extends ErrorCode
  }


  case class OAuthCredentials(
    clientId: String,
    clientSecret: String)

  case class OAuthEndpoint(
    key: String,
    scopes: List[String],
    authorizationUri: String,
    tokenUri: String
  )

  case class AccessToken(
    value: String,
    expiresIn: Long,
    tokenType: String
  )

  case class TokenResponse(
    idToken: String,
    accessToken: AccessToken,
    refreshToken: Option[String]
  )

  type AuthorizationRequestUrl = String

  case class AuthorizationResponse(
    code: String,
    state: String
  )

}

trait OAuthInstances extends OAuthTypes {

  implicit val tokenResponseRead = readE[TokenResponse] { jv =>
    if ((jv \ "error").isEmpty) {

      for {
        accessToken <- (jv \ "access_token").read[String]
        idToken <- (jv \ "id_token").read[String]
        expiresIn <- (jv \ "expires_in").read[Long]
        tokenType <- (jv \ "token_type").read[String]
        refreshToken <- (jv \ "refresh_token").read[Option[String]]
      } yield {
        TokenResponse(idToken, AccessToken(accessToken, expiresIn, tokenType), refreshToken)
      }

    } else {

      UncategorizedError("token_response_read_error", "Token response has an error", Nil).left

    }
  }

  implicit val tokenResponseErrorRead = read[ErrorCode.TokenResponseError] { json =>

    val facebookError = read[ErrorCode.TokenResponseError] { json =>
      ((json \ "error" \ "code").validate[String] |@| (json \ "error" \ "message").validate[String])(ErrorCode.TokenResponseError)
    }

    val googleError = read[ErrorCode.TokenResponseError] { json =>
      ((json \ "error").validate[String] |@| (json \ "error_description").validate[String])(ErrorCode.TokenResponseError)
    }

    (facebookError | googleError).read(json)

  }

}

object Encoding {

  def formEncodedStringToMap(s: String): Map[String, String] = Try {
    s.split("&").toList flatMap { s =>
      s.split("=").toList match {
        case key :: value :: Nil => Some(UrlCodingUtils.urlDecode(key) -> UrlCodingUtils.urlDecode(value))
        case _ => None
      }
    } toMap
  } getOrElse Map[String, String]()

  val toSkip = BitSet((('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9') ++ "!$'()*+,;:/?@-._~".toSet).map(_.toInt): _*)
  def encode(s: String) = UrlCodingUtils.urlEncode(s, toSkip = toSkip)
  def encodeMap(s: Map[String, String]) = s map { case (key, value) => f"${encode(key)}=${encode(value)}" } mkString "&"

}

object TaskC {

  import scala.concurrent.{Future => SFuture}
  import scalaz.concurrent.{Task => ZTask}

  def toTask[T](ft: => SFuture[T]): ZTask[T] = {
    import scalaz._
    ZTask.async { register =>
      ft.onComplete({
        case scala.util.Success(v) => register(\/-(v))
        case scala.util.Failure(ex) => register(-\/(ex))
      })
    }
  }

}



object OAuth extends OAuthTypes with OAuthInstances {

  import Encoding._

  def exchangeCodeForAccessToken(endpoint: OAuthEndpoint, credentials: OAuthCredentials, code: String, redirectUri: String): Future[ErrorCode \/ TokenResponse] = {
    val bodyParams = Map(
      "code" -> code,
      "client_id" -> credentials.clientId,
      "client_secret" -> credentials.clientSecret,
      "redirect_uri" -> redirectUri,
      "grant_type" -> "authorization_code")

    Http(url(endpoint.tokenUri) << bodyParams > (r => parseTokenResponse(r.getResponseBody)))

  }

  def parseTokenResponse(str: String): ErrorCode \/ TokenResponse =
    parseJsonOpt(str).fold(parseFormEncodedToken(str))(parseJsonToken)

  // parse either a TokenResponse or an ErrorCode.TokenResponseError
  def parseJsonToken(json: JValue): ErrorCode \/ TokenResponse = {
    json.read[TokenResponse] leftAs {
      (json.read[ErrorCode.TokenResponseError] leftAs ErrorCode.ParseError).merge
    }
  }

  // facebook api returns response as application/x-www-form-urlencoded
  // access_token=...&expires=...
  def parseFormEncodedToken(str: String): ErrorCode \/ TokenResponse = {
    val params = formEncodedStringToMap(str)

    if (params.isDefinedAt("access_token")) {
      val token = params("access_token")
      TokenResponse("", AccessToken(token, 0, ""), None).right
    } else {
      ErrorCode.InvalidResponse("malformed_message").left
    }
  }


  // success case
  // https://www.example.org/oauth2callback?
  //   state=security_token%3D...%26action%3Dlogin&
  //   code=...&
  //   authuser=0&
  //   num_sessions=1&
  //   prompt=consent&
  //   session_state=...
  //
  // error case
  // https://www.example.org/oauth2callback?error=access_denied&state=security_token%3D...%26action%3Dlogin
  def parseAuthorizationResponse(params: Map[String, String]): ErrorCode \/ AuthorizationResponse = {
    if (params.isDefinedAt("error")) {
      val error = params("error")
      ErrorCode.AuthorizationError(error, params.getOrElse("description", "")).left
    } else {
      if (!params.isDefinedAt("code")) {
        ErrorCode.InvalidResponse("missing code parameter").left
      } else {
        val code = params("code")
        val state = params.getOrElse("state", "")
        AuthorizationResponse(code, state).right
      }
    }
  }

  def authorizationRequestUrl(endpoint: OAuthEndpoint, credentials: OAuthCredentials, redirectUri: String, state: Map[String, String]): AuthorizationRequestUrl = {
    val queryParams = Map(
      "client_id" -> credentials.clientId,
      "response_type" -> "code",
      "scope" -> endpoint.scopes.mkString(" "),
      "redirect_uri" -> redirectUri,
      "state" -> encodeMap(state))

    s"${endpoint.authorizationUri}?${encodeMap(queryParams)}"
  }

  def authenticateUser(endpoint: OAuthEndpoint, credentials: OAuthCredentials, redirectUri: String): Task[ErrorCode \/ TokenResponse] = {
    val req1Url = authorizationRequestUrl(endpoint, credentials, redirectUri, Map("action" -> "login", "provider" -> endpoint.key))

    println(req1Url)
    println("Enter code:")
    val code = readLine

    TaskC.toTask(exchangeCodeForAccessToken(endpoint, credentials, code, redirectUri))
  }


}
