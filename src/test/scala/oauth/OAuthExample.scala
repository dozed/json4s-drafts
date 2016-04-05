package oauth

import scalaz._, Scalaz._

object OAuthExample extends App  {

  import OAuth._
  import OAuthApis._

  val redirectUri = "http://local.mindool.com/oauth2callback"

  // facebook uses a proprietary oauth 2.0 authentication extension
  def fb = OAuthEndpoint("facebook", List("email", "public_profile"), "https://www.facebook.com/dialog/oauth", "https://graph.facebook.com/oauth/access_token")
  def fbCreds = OAuthCredentials(???, ???)

  // openid authentication https://accounts.google.com/.well-known/openid-configuration
  val google = OAuthEndpoint("google", List("email", "profile"), "https://accounts.google.com/o/oauth2/v2/auth", "https://www.googleapis.com/oauth2/v4/token")
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
