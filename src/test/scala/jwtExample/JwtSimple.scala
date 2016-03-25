package jwtExample

import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec
import java.util.Base64

import scalaz._
import Scalaz._
import org.json4s._
import org.json4s.jackson.{compactJson, parseJson, parseJsonOpt, prettyJson}
import org.json4s.ext.scalaz.JsonScalaz._
import shapeless._
import shapeless.newtype._
import shapeless.{:+:, Coproduct, _}
import shapeless.syntax.std.tuple._
import shapeless.syntax.std.traversable._

import scala.util.Try

object JwtSimple extends App {

  // types

  case class Jws[A:JSONW](
    header: List[Header],
    payload: A,
    signature: JwsSignature
  ) {

    lazy val alg: Algorithm = header.collectFirst { case Header.Alg(x) => x}.get

  }

  type JwsCompact = String
  type JwsSignature = String
  type Jwt = List[Claim]
  type JoseHeader = List[Header]

  sealed trait Header

  object Header {

    case class Typ(value: String) extends Header
    case class Cty(value: String) extends Header
    case class Alg(value: Algorithm) extends Header

  }




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
    // TODO value
    case class Custom(name: String, value: String) extends Claim

  }





  // type StringOrList = Coproduct.`String, List[String]`.T
  type StringOrList = String :+: List[String] :+: CNil

  def stringOrList = Coproduct[StringOrList]



  sealed trait JwsError

  object JwsError {
    case object NoAlgHeader extends JwsError
    case object InvalidJwsCompact extends JwsError
    case object InvalidSignature extends JwsError
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

  implicit val algorithmShow = Show.show[Algorithm] {
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


  object Jws {

    def validate[A](compact: JwsCompact): JwsError \/ Unit = {
      for {
        jws1 <- Jws.decode[List[Claim]](compact) \/> JwsError.InvalidJwsCompact
        jws2 = Jwt.sign(jws1.payload, secret, jws1.alg)
        _ <- {
          if (jws1.signature === jws2.signature) ().right
          else JwsError.InvalidSignature.left
        }
      } yield ()
    }

    def decode[A:JSONR:JSONW](jws: JwsCompact): Option[Jws[A]] = {
      val xs = jws.split('.').toList
      for {
        (headerText, payloadText, signature) <- xs.toHList[String :: String :: String :: HNil].map(_.tupled)
        header <- decodeFromBase64[List[Header]](headerText)
        claim <- decodeFromBase64[A](payloadText)
        // alg <- headers.collectFirst { case Header.Alg(x) => x }  // JWS only valid with an alg header ?
      } yield {
        Jws(header, claim, signature)
      }
    }

    def encodeHeaderAndPayload[A:JSONW](header: List[Header], payload: A): String = {
      val encodedHeader = encodeBase64Url(compactJson(header.toJson))
      val encodedPayload = encodeBase64Url(compactJson(payload.toJson))
      val encodedHeaderAndPayload = s"$encodedHeader.$encodedPayload"

      encodedHeaderAndPayload
    }

    def computeMac(encodedHeaderAndPayload: String, algorithm: Algorithm, secret: String): JwsSignature = {
      def hmac(alg: Algorithm) = {
        val mac: Mac = Mac.getInstance(alg.shows)
        mac.init(new SecretKeySpec(secret.getBytes("utf-8"), alg.shows))
        encodeBase64Url(mac.doFinal(encodedHeaderAndPayload.getBytes("utf-8")))
      }

      algorithm match {
        case Algorithm.HS256 => hmac(Algorithm.HS256)
        case Algorithm.HS384 => hmac(Algorithm.HS384)
        case Algorithm.HS512 => hmac(Algorithm.HS512)
        case Algorithm.NONE => ""
      }
    }

    def decodeFromBase64[A:JSONR](base64: String): Option[A] = {
      for {
        text <- Try(Base64.getDecoder.decode(base64)).toOption
        json <- parseJsonOpt(new String(text))
        a <- json.validate[A].toOption
      } yield a
    }

    def decodeBase64(subject: String): String = new String(Base64.getDecoder.decode(subject))

    def encodeBase64Url(subject: String): String = Base64.getEncoder.encodeToString(subject.getBytes("utf-8"))

    def encodeBase64Url(subject: Array[Byte]): String = Base64.getEncoder.encodeToString(subject)

  }

  object Jwt {

    def sign(claims: List[Claim], secret: String, alg: Algorithm): Jws[Jwt] = {
      val headers = List[Header](
        Header.Typ("JWT"),
        Header.Alg(alg)
      )

      val headerAndPayload = Jws.encodeHeaderAndPayload(headers, claims)
      val mac = Jws.computeMac(headerAndPayload, alg, secret)
      Jws[Jwt](headers, claims, mac)
    }

    def compact(claims: List[Claim], secret: String, alg: Algorithm): JwsCompact = {
      val jws = sign(claims, secret, alg)
      val headerAndPayload = Jws.encodeHeaderAndPayload(jws.header, jws.payload)
      s"$headerAndPayload.${jws.signature}"
    }

  }

  // example

  val jws1 = Jws[Jwt](
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
    ),
    "foo"
  )



  val json = jws1.toJson
  val sig2 = json.read[Jws[Jwt]] getOrElse ???

  println(prettyJson(json))
  // TODO order of headers/claims
  println(jws1 == sig2)
  println(jws1)
  println(sig2)

  val secret = "thequickbrownfoxjumpsoverthelazydog"

  println(Jwt.sign(jws1.payload, secret, Algorithm.HS512))

  val s = "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzUxMiJ9.eyJpc3MiOiJpc3MiLCJzdWIiOiJzdWIiLCJhdWQiOiJhdWQiLCJhdWQiOlsiYXVkIiwiYXVkMiJdLCJleHAiOjQyLCJuYmYiOjQyLCJpYXQiOjQyLCJqdGkiOiJqdGkiLCJmb28iOiJiYXIifQ==.9SCDyruJ9p0SGkzGdMdBc6O5wLK1G7MKtGuNABVEUBnyMDI1HNPo3BRAQhxgylA+cmdjEyeq6FTKY62r0sBemw=="
  val s2 = "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzUxMiJ9.eyJpc3MiOiJpc3MiLCJzdWIiOiJzdWIiLCJhdWQiOiJhdWQiLCJhdWQiOlsiYXVkIiwiYXVkMiJdLCJleHAiOjQyLCJuYmYiOjQyLCJpYXQiOjQyLCJqdGkiOiJqdGkiLCJmb28iOiJiYXIifQ==.9SCDyruJ9p0SGkzGdMdBc6O5wLK1G7MKtGuNABVEUBnyMDI1HNPo3BRAQhxgylA+cmdjEyeq6FTKY62r0sBw=="
  val s3 = "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzUxMiJ9eyJpc3MiOiJpc3MiLCJzdWIiOiJzdWIiLCJhdWQiOiJhdWQiLCJhdWQiOlsiYXVkIiwiYXVkMiJdLCJleHAiOjQyLCJuYmYiOjQyLCJpYXQiOjQyLCJqdGkiOiJqdGkiLCJmb28iOiJiYXIifQ==.9SCDyruJ9p0SGkzGdMdBc6O5wLK1G7MKtGuNABVEUBnyMDI1HNPo3BRAQhxgylA+cmdjEyeq6FTKY62r0sBw=="


  println(s)


  val r1 = Jws.validate(s)
  val r2 = Jws.validate(s2)
  val r3 = Jws.validate(s3)


  println(r1)  // \/-(())
  println(r2)  // -\/(InvalidSignature)
  println(r3)  // -\/(InvalidJwsCompact)


}
