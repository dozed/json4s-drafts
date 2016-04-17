package oauth

import scalaz._, Scalaz._

import dispatch.Defaults._
import dispatch._
import org.json4s.ext.scalaz.JsonScalaz._
import org.json4s.jackson.parseJson

import scalaz.Scalaz._
import scalaz._
import scalaz.concurrent.Task

object OAuthApis {

  import OAuth._

  case class UserProfile(
    id: String,
    email: String,
    verifiedEmail: Boolean,
    name: String,
    givenName: String,
    familyName: String,
    link: String,
    pictureOpt: Option[String],
    gender: String
  )

  def lookupProfileReader(s: String): JSONR[UserProfile] = s match {
    case "facebook" => readFacebookUser
    case "google" =>  readGoogleUser
  }

  def lookupProfileApi(s: String) = s match {
    case "facebook" => "https://graph.facebook.com/me"
    case "google" =>  "https://www.googleapis.com/oauth2/v3/userinfo"
  }

  // person's profile in OpenID Connect format
  // https://developers.google.com/+/web/api/rest/openidconnect/getOpenIdConnect#request
  // https://accounts.google.com/.well-known/openid-configuration
  val readGoogleUser = JSONR.instanceE[UserProfile] { jv =>
    for {
      id <- (jv \ "sub").read[String]
      email <- (jv \ "email").read[String]
      verifiedEmail <- (jv \ "email_verified").read[Boolean]
      name <- (jv \ "name").read[String]
      givenName <- (jv \ "given_name").read[String]
      familyName <- (jv \ "family_name").read[String]
      link <- (jv \ "profile").read[String]
      picture <- (jv \ "picture").read[String]
      gender <- (jv \ "gender").read[String]
    } yield {
      UserProfile(id, email, verifiedEmail, name, givenName, familyName, link, picture.some, gender)
    }
  }

  val readFacebookUser = JSONR.instanceE[UserProfile] { jv =>
    for {
      id <- (jv \ "id").read[String]
      email <- (jv \ "email").read[String]
      verified <- (jv \ "verified").read[Boolean]
      name <- (jv \ "name").read[String]
      firstName <- (jv \ "first_name").read[String]
      lastName <- (jv \ "last_name").read[String]
      link <- (jv \ "link").read[String]
      gender <- (jv \ "gender").read[String]
    } yield {
      UserProfile(id, email, verified, name, firstName, lastName, link, f"https://graph.facebook.com/v2.3/$id/picture".some, gender)
    }
  }

  def fetchUserProfile(endpoint: OAuthEndpoint, accessToken: AccessToken): Task[ErrorCode \/ UserProfile] = {
    val apiEndpoint = lookupProfileApi(endpoint.key)
    val reader = lookupProfileReader(endpoint.key)

    TaskC.toTask {
      val h = Map("Authorization" -> f"Bearer ${accessToken.value}")
      Http(url(apiEndpoint) <:< h OK as.String).map { data =>
        parseJson(data).read[UserProfile](reader) leftAs (ErrorCode.ParseError:ErrorCode)
      }
    }
  }

}

