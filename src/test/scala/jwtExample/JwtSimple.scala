package jwtExample

import scalaz._, Scalaz._

import org.json4s._
import org.json4s.jackson.{prettyJson, compactJson, parseJson, parseJsonOpt}
import org.json4s.ext.scalaz.JsonScalaz._

import shapeless._
import shapeless.newtype._
import shapeless.{:+:, Coproduct, _}

object JwtSimple extends App {

  // types

  case class Jwt(headers: List[Header], claims: List[Claim])

  // type StringOrList = Coproduct.`String, List[String]`.T
  type StringOrList = String :+: List[String] :+: CNil

  def stringOrList = Coproduct[StringOrList]

  sealed trait Algorithm

  object Algorithm {

    case object HS256 extends Algorithm
    case object HS384 extends Algorithm
    case object HS512 extends Algorithm
    case object NONE extends Algorithm

  }


  sealed trait Claim

  object Claim {

    case class Iss(value: String) extends Claim
    case class Sub(value: String) extends Claim
    case class Aud(value: StringOrList) extends Claim
    case class Exp(value: Long) extends Claim
    case class Nbf(value: Long) extends Claim
    case class Iat(value: Long) extends Claim
    case class Jti(value: String) extends Claim

    // user-defined claims
    case class Custom(name: String, value: String) extends Claim

  }


  sealed trait Header
  object Header {

    case class Typ(value: String) extends Header
    case class Cty(value: String) extends Header
    case class Alg(value: Algorithm) extends Header

  }



  // instances

  implicit lazy val readAud =
    readL[List[String]].map(x => Claim.Aud(stringOrList(x))) orElse
      readL[String].map(x => Claim.Aud(stringOrList(x)))

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

  implicit val algorithmShows = Show.show[Algorithm] {
    case Algorithm.HS256 => "HmacSHA256"
    case Algorithm.HS384 => "HmacSHA384"
    case Algorithm.HS512 => "HmacSHA512"
    case Algorithm.NONE => "none"
  }


  val readHeader: (String, JValue) => Result[Header] = {
    case ("typ", v) => v.validate[String].map(Header.Typ)
    case ("cty", v) => v.validate[String].map(Header.Cty)
    case ("alg", v) => v.validate[Algorithm].map(Header.Alg)
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



  // example

  val token = Jwt(
    List(
      Header.Typ("foo"),
      Header.Cty("bar"),
      Header.Alg(Algorithm.HS384)
    ),
    List(
      Claim.Iss("iss"),
      Claim.Sub("sub"),
      Claim.Aud(Coproduct[StringOrList]("aud")),
      Claim.Aud(Coproduct[StringOrList](List("aud", "aud2"))),
      Claim.Exp(42),
      Claim.Nbf(42),
      Claim.Iat(42),
      Claim.Jti("jti"),
      Claim.Custom("foo", "\"bar\"")
    )
  )



  val json = token.toJson
  val token2 = json.read[Jwt] getOrElse ???

  println(prettyJson(json))
  // TODO order of headers/claims
  println(token === token2)
  println(token)
  println(token2)






}
