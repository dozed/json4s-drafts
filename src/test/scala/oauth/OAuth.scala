package oauth

import org.json4s._
import org.json4s.jackson.{parseJson, parseJsonOpt}
import org.json4s.ext.scalaz.JsonScalaz._

import scala.concurrent.Future
import scalaz.concurrent.Task

import dispatch._, Defaults._

import scalaz._, Scalaz._

object OAuth extends OAuthTypes with OAuthJSONR with OAuthRequestParser {

  def exchangeCodeForAccessToken(endpoint: OAuthEndpoint, credentials: OAuthCredentials, code: String, redirectUri: String): Future[ErrorCode \/ TokenResponse] = {
    val bodyParams = Map(
      "code" -> code,
      "client_id" -> credentials.clientId,
      "client_secret" -> credentials.clientSecret,
      "redirect_uri" -> redirectUri,
      "grant_type" -> "authorization_code")

    Http(url(endpoint.tokenUri) << bodyParams > (r => parseTokenResponse(r.getResponseBody)))

  }

  def authorizationRequestUrl(endpoint: OAuthEndpoint, credentials: OAuthCredentials, redirectUri: String, state: String): AuthorizationRequestUrl = {
    val queryParams = Map(
      "client_id" -> credentials.clientId,
      "response_type" -> "code",
      "scope" -> endpoint.scopes.mkString(" "),
      "redirect_uri" -> redirectUri,
      "state" -> state)

    s"${endpoint.authorizationUri}?${UrlEncoding.encodeMap(queryParams)}"
  }

  def authenticateUser(endpoint: OAuthEndpoint, credentials: OAuthCredentials, redirectUri: String, state: String): Task[ErrorCode \/ TokenResponse] = {
    val req1Url = authorizationRequestUrl(endpoint, credentials, redirectUri, state)

    println(req1Url)
    println("Enter code:")
    val code = readLine

    TaskC.toTask(exchangeCodeForAccessToken(endpoint, credentials, code, redirectUri))
  }


}

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

trait OAuthJSONR extends OAuthTypes {

  implicit val tokenResponseRead = JSON.readE[TokenResponse] { jv =>
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

  implicit val tokenResponseErrorRead = JSON.read[ErrorCode.TokenResponseError] { json =>

    val facebookError = JSON.read[ErrorCode.TokenResponseError] { json =>
      ((json \ "error" \ "code").validate[String] |@| (json \ "error" \ "message").validate[String]) (ErrorCode.TokenResponseError)
    }

    val googleError = JSON.read[ErrorCode.TokenResponseError] { json =>
      ((json \ "error").validate[String] |@| (json \ "error_description").validate[String]) (ErrorCode.TokenResponseError)
    }

    val r: JSONR[ErrorCode.TokenResponseError] = (facebookError orElse googleError)

    r.read(json)

  }

}

trait OAuthRequestParser extends OAuthTypes with OAuthJSONR {

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
    val params = UrlEncoding.formEncodedStringToMap(str)

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

}

