package oauth

import scalaz._, Scalaz._

object OAuthExample extends App  {

  import OAuth._
  import OAuthApis._

  val redirectUri = "http://local.mindool.com/oauth2callback"

  val fb = OAuthEndpoint("facebook", List("email"), "https://www.facebook.com/dialog/oauth", "https://graph.facebook.com/oauth/access_token")
  val fbCreds = OAuthCredentials(???, ???)

  val google = OAuthEndpoint("google", List("email"), "https://accounts.google.com/o/oauth2/auth", "https://www.googleapis.com/oauth2/v3/token")
  val googleCreds = OAuthCredentials(???, ???)

  val res1 = (for {
    token <- EitherT(authenticateUser(google, googleCreds, redirectUri))
    user <- EitherT(OAuthApis.fetchUserProfile(google, token.accessToken))
  } yield user).run.run

  println(res1)

  val res2 = (for {
    token <- EitherT(authenticateUser(fb, fbCreds, redirectUri))
    user <- EitherT(OAuthApis.fetchUserProfile(fb, token.accessToken))
  } yield user).run.run

  println(res2)


}
