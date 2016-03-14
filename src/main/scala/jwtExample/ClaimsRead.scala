package jwtExample

import drafts.ReadExt.{unexpected, _}
import shapeless.newtype._
import shapeless.{:+:, Coproduct, _}

import _root_.scalaz._, Scalaz._

import org.json4s._
import org.json4s.jackson.{compactJson, parseJson}
import org.json4s.scalaz.JsonScalaz._

object ClaimsRead {

  import ClaimsTypes._

  // read for newtype
  implicit lazy val readSub =
    readL[List[String]].map(x => Claim.Sub(stringOrList(x))) orElse
      readL[String].map(x => Claim.Sub(stringOrList(x)))

  val readClaim: (String, JValue) => Result[Claim] = {
    case ("iss", v) => v.validateC[Claim.Iss, Claim]
    case ("sub", v) => v.validateC[Claim.Sub, Claim]
    case ("aud", v) => v.validateC[Claim.Aud, Claim]
    case ("exp", v) => v.validateC[Claim.Exp, Claim]
    case ("nbf", v) => v.validateC[Claim.Nbf, Claim]
    case ("iat", v) => v.validateC[Claim.Iat, Claim]
    case ("jti", v) => v.validateC[Claim.Jti, Claim]
    case (key, value) => claim(Custom(key, compactJson(value))).successNel
  }

  implicit lazy val claimsRead = read[List[Claim]] {
    case x: JObject => x.obj.map(x => readClaim.tupled(x)).sequence[Result, Claim]
    case json => unexpected(json, classOf[JObject])
  }

}
