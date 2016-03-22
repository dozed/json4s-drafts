package jwtExample

import shapeless.newtype._
import shapeless.{:+:, Coproduct, _}

import scalaz._, Scalaz._
import org.json4s._
import org.json4s.ext.scalaz.JsonScalaz._
import org.json4s.jackson.{compactJson, parseJson}

object JwtJSON {

  import JwtTypes._

  implicit lazy val readAud =
    readL[List[String]].map(x => Claim.Aud(stringOrList(x))) orElse
      readL[String].map(x => Claim.Aud(stringOrList(x)))

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

  val writeClaim: Claim => (String, JValue) = {
    case Claim.Iss(x) => ("iss", x.toJson)
    case Claim.Sub(x) => ("sub", x.toJson)
    case Claim.Aud(x) => ("aud", x.toJson)
    case Claim.Exp(x) => ("exp", x.toJson)
    case Claim.Nbf(x) => ("nbf", x.toJson)
    case Claim.Iat(x) => ("iat", x.toJson)
    case Claim.Jti(x) => ("jti", x.toJson)
    case Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inl(Custom(key, value))))))))) => (key, parseJson(value))
  }

  implicit lazy val claimsRead = read[List[Claim]] {
    case x: JObject => x.obj.map(x => readClaim.tupled(x)).sequence[Result, Claim]
    case json => Fail.unexpected(json, classOf[JObject])
  }

  implicit lazy val claimsWrite = write[List[Claim]] { xs =>
    JObject(xs map writeClaim)
  }

}