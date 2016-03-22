package jwtExample

import shapeless.newtype._
import shapeless.{:+:, Coproduct, _}

import org.json4s._
import org.json4s.ext.scalaz.JsonScalaz._
import org.json4s.jackson.{compactJson, parseJson}

import scalaz._, Scalaz._

object Jwt extends App {

  import JwtTypes._
  import JwtJSON._

  val header = parseJson(
    """
      |{
      |  "typ": "JWT",
      |  "alg": "HS256"
      |}
    """.stripMargin)


  val claimsJson = parseJson(
    """
      |{
      |  "iss": "joe",
      |  "exp": 1300819380,
      |  "http://example.com/is_root": true,
      |  "view": [100,200,300],
      |  "admin": [100,200,300]
      |}
    """.stripMargin)


  val cl1: Claim = Coproduct.apply[Claim](Claim.Iss("asd"))
  val cl2: Claim = claim(Claim.Iss("asd"))

  val claims: List[Claim] = List(claim(Claim.Iss("hello")))
  val iss = claims.head.select[Claim.Iss]
  println(iss)


  val x = claimsJson.read[List[Claim]]

  println(x)

  // TODO compiler hangs
  // x.toJson

  println((x getOrElse ???).toJson)

}

