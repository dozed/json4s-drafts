package jwt

import java.util.Base64
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

import org.json4s._
import org.json4s.ext.scalaz.JsonScalaz._
import org.json4s.jackson.{compactJson, parseJsonOpt}
import shapeless.syntax.std.traversable._
import shapeless.syntax.std.tuple._
import shapeless.{:+:, Coproduct, _}

import scala.util.Try
import scalaz.Scalaz._
import scalaz._


// JSON Web Signature (JWS)
// https://tools.ietf.org/html/rfc7515

trait JwsTypes {

   // TODO the payload can contain an arbitrary sequence of octets
  case class Jws[A: JSONW](
    header: List[Header],
    payload: A,
    signature: JwsSignature
  ) {

    lazy val alg: Algorithm = header.collectFirst { case Header.Alg(x) => x }.get

  }

  implicit def jwsEqual[A:Equal] = Equal.equal[Jws[A]] {
    (a, b) =>
      // ISet.fromList(a.header) === ISet.fromList(b.header) &&
      a.header === b.header &&
      a.signature === b.signature &&
      a.payload === b.payload
  }

  implicit def jwsShow[A] = Show.showFromToString[Jws[A]]

  implicit val headerEqual = Equal.equalA[Header]

  object Jws

  type JoseHeader = List[Header]
  type JwsCompact = String
  type JwsSignature = String

    // TODO only valid with an alg header

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


  sealed trait JwsError

  object JwsError {

    case object NoAlgHeader extends JwsError
    case object InvalidJwsCompact extends JwsError
    case object InvalidSignature extends JwsError
    case object NoneNotSupported extends JwsError
    case class MacError(msg: String) extends JwsError

  }

}

trait JwsOperations {

  implicit class JwsExt[A:JSONW](jws: Jws[A]) {

    def compact: JwsCompact = {
      val headerAndPayload = Jws.encodeHeaderAndPayload(jws.header, jws.payload)
      s"$headerAndPayload.${jws.signature}"
    }

  }

  implicit class JwsCompanionExt(companion: Jws.type) {

    def decode[A: JSONR : JSONW](jws: JwsCompact): Option[Jws[A]] = {
      val xs = jws.split('.').toList
      for {
        (headerText, payloadText, signature) <- xs.toHList[String :: String :: String :: HNil].map(_.tupled)
        header <- decodeFromBase64[List[Header]](headerText)
        claim <- decodeFromBase64[A](payloadText)
      // alg <- headers.collectFirst { case Header.Alg(x) => x }
      } yield {
        Jws(header, claim, signature)
      }
    }

    def encodeHeaderAndPayload[A: JSONW](header: List[Header], payload: A): String = {
      val encodedHeader = encodeBase64Url(compactJson(header.toJson))
      val encodedPayload = encodeBase64Url(compactJson(payload.toJson))
      val encodedHeaderAndPayload = s"$encodedHeader.$encodedPayload"

      encodedHeaderAndPayload
    }

    def computeMac(encodedHeaderAndPayload: String, algorithm: Algorithm, secret: String): JwsError \/ JwsSignature = {
      def hmac(alg: String): JwsError \/ JwsSignature = Try {
        val mac: Mac = Mac.getInstance(alg)
        mac.init(new SecretKeySpec(secret.getBytes("utf-8"), alg))
        encodeBase64Url(mac.doFinal(encodedHeaderAndPayload.getBytes("utf-8")))
      } cata (_.right, t => JwsError.MacError(t.getMessage).left)

      algorithm match {
        case Algorithm.HS256 => hmac("HmacSHA256")
        case Algorithm.HS384 => hmac("HmacSHA384")
        case Algorithm.HS512 => hmac("HmacSHA512")
        case Algorithm.NONE => JwsError.NoneNotSupported.left
      }
    }

    def decodeFromBase64[A: JSONR](base64: String): Option[A] = {
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

}

trait JwsJSONInstances {

  implicit lazy val algorithmRead: JSONR[Algorithm] = JSON.readL[String].map(_.toUpperCase).emap {
    case "HS256" => Algorithm.HS256.successNel
    case "HS384" => Algorithm.HS384.successNel
    case "HS512" => Algorithm.HS512.successNel
    case "NONE" => Algorithm.NONE.successNel
    case x => Fail.apply("", "one of: HS256, HS384, HS512, NONE", List(x))
  }

  implicit lazy val algorithmWrite: JSONW[Algorithm] = JSON.writeL[String].contramap[Algorithm] {
    case Algorithm.HS256 => "HS256"
    case Algorithm.HS384 => "HS384"
    case Algorithm.HS512 => "HS512"
    case Algorithm.NONE => "NONE"
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

  implicit lazy val headersRead: JSONR[List[Header]] = JSON.read[List[Header]] {
    case x: JObject => x.obj.map(x => readHeader.tupled(x)).sequence[Result, Header]
    case json => Fail.unexpected(json, classOf[JObject])
  }

  implicit lazy val headersWrite: JSONW[List[Header]] = JSON.write[List[Header]] { xs =>
    JObject(xs map writeHeader)
  }


  // TODO simplify 1:1 mapping
  // TODO fix DSL, should work without toJson
  implicit def jwsJson[A: JSONR : JSONW]: JSON[Jws[A]] = new JSON[Jws[A]] {
    override def write(value: Jws[A]): JValue = {
      ("header" -> value.header.toJson) ~
      ("payload" -> value.payload) ~
      ("signature" -> value.signature)
    }

    override def read(json: JValue): Result[Jws[A]] = {

      (for {
        header <- (json \ "header").read[List[Header]]
        payload <- (json \ "payload").read[A]
        signature <- (json \ "signature").read[JwsSignature]
      } yield {
        Jws(header, payload, signature)
      }).validationNel

    }
  }


}
