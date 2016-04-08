package oauth

import org.json4s.ext.scalaz.JsonScalaz._

import scalaz._, Scalaz._

sealed trait DeferredAction
case object Login extends DeferredAction
case class Create(text: String) extends DeferredAction

object OAuthExample extends App  {

  import OAuth._
  import OAuthApis._

  val redirectUri = "http://local.mindool.com/oauth2callback"


  case class OAuthState(action: DeferredAction, endpoint: String)

  def fb = OAuthEndpoint("facebook", List("email", "public_profile"), "https://www.facebook.com/dialog/oauth", "https://graph.facebook.com/oauth/access_token")
  def fbCreds = OAuthCredentials(???, ???)

  val google = OAuthEndpoint("google", List("email", "profile"), "https://accounts.google.com/o/oauth2/v2/auth", "https://www.googleapis.com/oauth2/v4/token")
  val googleCreds = OAuthCredentials(???, ???)


  implicit val deferredActionJson = deriveJSON[DeferredAction]
  implicit val stateJson = deriveJSON[OAuthState]

  val res1 = (for {
    token <- EitherT(authenticateUser(google, googleCreds, redirectUri, OAuthState(Login, "google").toJson.nospaces))
    user <- EitherT(OAuthApis.fetchUserProfile(google, token.accessToken))
  } yield user).run.run

  println(res1)

  val res2 = (for {
    token <- EitherT(authenticateUser(fb, fbCreds, redirectUri, OAuthState(Create("foo title"), "facebook").toJson.nospaces))
    user <- EitherT(OAuthApis.fetchUserProfile(fb, token.accessToken))
  } yield user).run.run

  println(res2)

}
