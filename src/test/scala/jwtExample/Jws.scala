package jwtExample

import java.util.Base64
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

import JwtTypes._
import JwtJSON._

import org.json4s._
import org.json4s.ext.scalaz.JsonScalaz._
import org.json4s.jackson.{parseJson, prettyJson, compactJson}
import shapeless.Coproduct
import scalaz._, Scalaz._

object Jws extends App {

  sealed trait JwtError
  object JwtError {
    case object NoAlgHeader extends JwtError
  }

  type Jws = String
  type HmacSignature = String

  def decodeBase64(subject: String): String = new String(Base64.getDecoder.decode(subject))

  def encodeBase64Url(subject: String): String = Base64.getEncoder.encodeToString(subject.getBytes("utf-8"))

  def encodeBase64Url(subject: Array[Byte]): String = Base64.getEncoder.encodeToString(subject)

  def signature(encodedHeaderAndPayload: String, algorithm: Algorithm, secret: String): String = {
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

  def encodeHeaderAndPayload(jwt: Jwt): String = {
    val encodedHeader: String = encodeBase64Url(compactJson(jwt.headers.toJson))
    val encodedPayload: String = encodeBase64Url(compactJson(jwt.claims.toJson))
    val encodedHeaderAndPayload: String = s"$encodedHeader.$encodedPayload"

    encodedHeaderAndPayload
  }

  def encodeAsJWS(jwt: Jwt, secret: String): JwtError \/ Jws = {
    for {
      alg <- jwt.headers.collectFirst { case Header.Alg(alg) => alg } \/> JwtError.NoAlgHeader
      enc <- encodeHeaderAndPayload(jwt).right
      hmac <- signature(enc, alg, secret).right
    } yield s"$enc.$hmac"
  }

  def signature(jwt: Jwt, secret: String): JwtError \/ HmacSignature = {
    for {
      alg <- jwt.headers.collectFirst { case Header.Alg(alg) => alg } \/> JwtError.NoAlgHeader
      enc <- encodeHeaderAndPayload(jwt).right
      hmac <- signature(enc, alg, secret).right
    } yield hmac
  }

}
