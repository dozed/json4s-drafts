package jwtExample

import shapeless.newtype._
import shapeless.{:+:, Coproduct, _}

import scalaz._, Scalaz._
import org.json4s._
import org.json4s.ext.scalaz.JsonScalaz._
import org.json4s.jackson.{compactJson, parseJson}

object JwtJSON {

  import JwtTypes._

  // read for newtype
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

  // TODO Any -> Claim
  val writeClaim: Any => (String, JValue) = {
    case x:Claim.Iss => ("iss", x.toJson)
    case x:Claim.Sub => ("sub", x.toJson)
    case x:Claim.Aud => ("aud", x.toJson)
    case x:Claim.Exp => ("exp", x.toJson)
    case x:Claim.Nbf => ("nbf", x.toJson)
    case x:Claim.Iat => ("iat", x.toJson)
    case x:Claim.Jti => ("jti", x.toJson)
    case Custom(key, value) => (key, parseJson(value))
  }

  implicit lazy val claimsRead = read[List[Claim]] {
    case x: JObject => x.obj.map(x => readClaim.tupled(x)).sequence[Result, Claim]
    case json => Fail.unexpected(json, classOf[JObject])
  }

  implicit lazy val claimsWrite = write[List[Claim]] { xs =>
    JObject(xs map writeClaim)
  }

}
