package oauth

import org.json4s._
import org.json4s.jackson.{parseJsonOpt}
import org.json4s.ext.scalaz.JsonScalaz._

import scala.concurrent.Future

// import scala.collection.immutable.BitSet
//import scala.util.Try
//import dispatch._, Defaults._
//import rl.UrlCodingUtils

import scalaz._, Scalaz._

object OAuth {

  trait ErrorCode

  object ErrorCode {
    case class InvalidResponse(msg: String) extends ErrorCode
    case class AuthorizationError(msg: String, description: String) extends ErrorCode
  }

  case class OAuthCredentials(
    clientId: String,
    clientSecret: String)

  case class AccessToken(
    accessToken: String,
    expiresIn: Long,
    tokenType: String)

  sealed trait TokenResponse

  case class TokenResponseSuccess(
    idToken: String,
    accessToken: AccessToken,
    refreshToken: Option[String]) extends TokenResponse

  case class TokenResponseError(msg: String, description: String) extends TokenResponse

  type AuthorizationRequestUrl = String

  case class AuthorizationRequest(
    clientId: String,
    responseType: String,
    scopes: List[String],
    redirectUri: String,
    state: Map[String, String],
    authorizationEndpointUri: String)

  case class AuthorizationResponse(
    code: String,
    state: String)

  case class OAuthEndpoint(
    key: String,
    scopes: List[String],
    uri: String
  )

  // read a TokenResponse
  //
  // success case
  //{
  //  "access_token": "...",
  //  "token_type": "Bearer",
  //  "expires_in": 3600,
  //  "id_token": "..."
  //}
  //
  // error case
  //{
  //  "error": "invalid_grant",
  //  "error_description": "Code was already redeemed."
  //}
  implicit val tokenResponseRead = readE[TokenResponse] { jv =>
    if ((jv \ "error").isEmpty) {

      for {
        accessToken <- (jv \ "access_token").read[String]
        idToken <- (jv \ "id_token").read[String]
        expiresIn <- (jv \ "expires_in").read[Long]
        tokenType <- (jv \ "token_type").read[String]
        refreshToken <- (jv \ "refresh_token").read[Option[String]]
      } yield {
        TokenResponseSuccess(idToken, AccessToken(accessToken, expiresIn, tokenType), refreshToken)
      }

    } else {

      val facebookParse = read[TokenResponse] { json =>
        ((json \ "error" \ "code").validate[String] |@| (json \ "error" \ "message").validate[String])(TokenResponseError)
      }

      val googleParse = read[TokenResponse] { json =>
        ((json \ "error").validate[String] |@| (json \ "error_description").validate[String])(TokenResponseError)
      }

      val errorParse = facebookParse | googleParse | (TokenResponseError("token response error", ""):TokenResponse).point[JSONR]

      errorParse.readE1(jv)

    }
  }


  // facebook api returns response as application/x-www-form-urlencoded
  // access_token=...&expires=...
  def parseTokenResponse(str: String): ErrorCode \/ TokenResponse = {
    val params = readFormEncodedStringToMap(str)

    if (params.isDefinedAt("access_token")) {

      val token = params("access_token")
      TokenResponseSuccess("", AccessToken(token, 0, ""), None).right

    } else {

      for {
        json <- parseJsonOpt(str) \/> ErrorCode.InvalidResponse("malformed_message")
        tokenResponse <- json.read[TokenResponse] leftAs ErrorCode.InvalidResponse("malformed_message")
      } yield tokenResponse

    }
  }

  def encodeQueryParams(s: Map[String, String]): String = ???
  def readFormEncodedStringToMap(s: String): Map[String, String] = ???

  def authorizationRequest(credentials: OAuthCredentials, endpoint: OAuthEndpoint, redirectUri: String, state: Map[String, String] = Map.empty): AuthorizationRequest = {
    AuthorizationRequest(
      credentials.clientId, "code", endpoint.scopes, redirectUri,
      Map("action" -> "login", "provider" -> endpoint.key) ++ state,
      endpoint.uri
    )
  }

  def authorizationRequestUrl(authorizationRequest: AuthorizationRequest): AuthorizationRequestUrl = {
    val queryParams = Map(
      "client_id" -> authorizationRequest.clientId,
      "response_type" -> authorizationRequest.responseType,
      "scope" -> authorizationRequest.scopes.mkString(" "),
      "redirect_uri" -> authorizationRequest.redirectUri,
      "state" -> encodeQueryParams(authorizationRequest.state))

    s"${authorizationRequest.authorizationEndpointUri}?${encodeQueryParams(queryParams)}"
  }

  def exchangeCodeForAccessToken(credentials: OAuthCredentials, code: String, redirectUri: String): Future[ErrorCode \/ TokenResponseSuccess] = {
    val bodyParams = Map(
      "code" -> code,
      "client_id" -> credentials.clientId,
      "client_secret" -> credentials.clientSecret,
      "redirect_uri" -> redirectUri,
      "grant_type" -> "authorization_code")

    // Http(tokenEndpoint << bodyParams > asTokenResponse)
    ???
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
