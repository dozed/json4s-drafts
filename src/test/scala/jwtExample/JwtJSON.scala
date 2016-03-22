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
    case (key, value) => claim(Claim.Custom(key, compactJson(value))).successNel
  }

  val writeClaim: Claim => (String, JValue) = {
    case Claim.Iss(x) => ("iss", x.toJson)
    case Claim.Sub(x) => ("sub", x.toJson)
    case Claim.Aud(x) => ("aud", x.toJson)
    case Claim.Exp(x) => ("exp", x.toJson)
    case Claim.Nbf(x) => ("nbf", x.toJson)
    case Claim.Iat(x) => ("iat", x.toJson)
    case Claim.Jti(x) => ("jti", x.toJson)
    case Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inl(Claim.Custom(key, value))))))))) => (key, parseJson(value))
  }

  implicit lazy val claimsRead = read[List[Claim]] {
    case x: JObject => x.obj.map(x => readClaim.tupled(x)).sequence[Result, Claim]
    case json => Fail.unexpected(json, classOf[JObject])
  }

  implicit lazy val claimsWrite = write[List[Claim]] { xs =>
    JObject(xs map writeClaim)
  }

  implicit lazy val algorithmRead: JSONR[Algorithm] = readL[String] map (_.toUpperCase) emap {
    case "HS256" => Algorithm.HS256.successNel
    case "HS384" => Algorithm.HS384.successNel
    case "HS512" => Algorithm.HS512.successNel
    case "NONE" => Algorithm.NONE.successNel
    case x => Fail.apply("", "one of: HS256, HS384, HS512, NONE", List(x))
  }

  implicit lazy val algorithmWrite: JSONW[Algorithm] = writeL[String].contramap[Algorithm] {
    case Algorithm.HS256 => "HS256"
    case Algorithm.HS384 => "HS384"
    case Algorithm.HS512 => "HS512"
    case Algorithm.NONE => "NONE"
  }

  val readHeader: (String, JValue) => Result[Header] = {
    case ("typ", v) => v.validateC[Header.Typ, Header]
    case ("cty", v) => v.validateC[Header.Cty, Header]
    case ("alg", v) => v.validateC[Algorithm, Header]
    case (key, value) => Fail(key, "expected one of: typ, cty, alg", List(value))
  }

  val writeHeader: Header => (String, JValue) = {
    case Header.Typ(x) => ("typ", x.toJson)
    case Header.Cty(x) => ("cty", x.toJson)
    case Header.Alg(x) => ("alg", x.toJson)
  }

  implicit lazy val headersRead = read[List[Header]] {
    case x: JObject => x.obj.map(x => readHeader.tupled(x)).sequence[Result, Header]
    case json => Fail.unexpected(json, classOf[JObject])
  }

  implicit lazy val headersWrite = write[List[Header]] { xs =>
    JObject(xs map writeHeader)
  }

}
