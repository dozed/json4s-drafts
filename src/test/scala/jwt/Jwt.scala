package jwt

import org.json4s._
import org.json4s.ext.scalaz.JsonScalaz._
import org.json4s.jackson.{compactJson, parseJsonOpt}

import shapeless.{:+:, Coproduct, _}
import shapeless.syntax.std.traversable._
import shapeless.syntax.std.tuple._

import scalaz.Scalaz._
import scalaz._

// JSON Web Token (JWT)
// https://tools.ietf.org/html/rfc7519

trait JwtTypes {

  type Jwt = Jws[List[Claim]]
  object Jwt

  type NumericDate = Long

  sealed trait Claim

  object Claim {

    case class Iss(value: String) extends Claim
    case class Sub(value: String) extends Claim
    case class Aud(value: StringOrList) extends Claim
    case class Exp(value: NumericDate) extends Claim
    case class Nbf(value: Long) extends Claim
    case class Iat(value: Long) extends Claim
    case class Jti(value: String) extends Claim

    // user-defined claims
    // TODO value
    case class Custom(name: String, value: String) extends Claim

  }

  implicit val claimEqual = Equal.equalA[Claim]
  implicit val claimShow = Show.showFromToString[Claim]


  // type StringOrList = Coproduct.`String, List[String]`.T
  type StringOrList = String :+: List[String] :+: CNil

  def stringOrList = Coproduct[StringOrList]

}

trait JwtOperations {

  implicit class JwtExt(_jwt: Jwt.type) {

    def decode(compact: JwsCompact): JwsError \/ Jwt = {
      Jws.decode[List[Claim]](compact) \/> JwsError.InvalidJwsCompact
    }

    def validate(compact: JwsCompact, secret: String): JwsError \/ Unit = {
      for {
        jws1 <- Jws.decode[List[Claim]](compact) \/> JwsError.InvalidJwsCompact
        jws2 <- Jwt.sign(jws1.payload, secret, jws1.alg)
        _ <- {
          if (jws1.signature === jws2.signature) ().right
          else JwsError.InvalidSignature.left
        }
      } yield ()
    }


    def sign(claims: List[Claim], secret: String, alg: Algorithm): JwsError \/ Jws[List[Claim]] = {
      val headers = List[Header](
        Header.Typ("JWT"),
        Header.Alg(alg)
      )

      val headerAndPayload = Jws.encodeHeaderAndPayload(headers, claims)
      Jws.computeMac(headerAndPayload, alg, secret) map { mac =>
        Jws[List[Claim]](headers, claims, mac)
      }
    }

    def compact(claims: List[Claim], secret: String, alg: Algorithm): JwsError \/ JwsCompact = {
      sign(claims, secret, alg) map (_.compact)
    }

  }

}

trait JwtJSONInstances {

  implicit lazy val readAud =
    JSONR[List[String]].map(x => Claim.Aud(stringOrList(x))) orElse
      JSONR[String].map(x => Claim.Aud(stringOrList(x)))

  val readClaim: (String, JValue) => Result[Claim] = {
    case ("iss", v) => v.validate[String].map(Claim.Iss)
    case ("sub", v) => v.validate[String].map(Claim.Sub)
    case ("aud", v) => v.validate[StringOrList].map(Claim.Aud)
    case ("exp", v) => v.validate[Long].map(Claim.Exp)
    case ("nbf", v) => v.validate[Long].map(Claim.Nbf)
    case ("iat", v) => v.validate[Long].map(Claim.Iat)
    case ("jti", v) => v.validate[String].map(Claim.Jti)
    case (key, value) => Claim.Custom(key, compactJson(value)).successNel
  }

  val writeClaim: Claim => (String, JValue) = {
    case Claim.Iss(x) => ("iss", x.toJson)
    case Claim.Sub(x) => ("sub", x.toJson)
    case Claim.Aud(x) => ("aud", x.toJson)
    case Claim.Exp(x) => ("exp", x.toJson)
    case Claim.Nbf(x) => ("nbf", x.toJson)
    case Claim.Iat(x) => ("iat", x.toJson)
    case Claim.Jti(x) => ("jti", x.toJson)
    case Claim.Custom(key, value) =>
      parseJsonOpt(value).fold((key, value.toJson))(json => (key, json))
  }

  implicit lazy val claimsRead: JSONR[List[Claim]] = JSONR.instance[List[Claim]] {
    case x: JObject => x.obj.map(x => readClaim.tupled(x)).sequence[Result, Claim]
    case json => Fail.unexpected(json, classOf[JObject])
  }

  implicit lazy val claimsWrite: JSONW[List[Claim]] = JSONW.instance[List[Claim]] { xs =>
    JObject(xs map writeClaim)
  }


}

