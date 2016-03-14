package jwtExample

import drafts.ReadExt.{unexpected, _}
import shapeless.newtype._
import shapeless.{:+:, Coproduct, _}

import scalaz.Scalaz._
import org.json4s._
import org.json4s.jackson.{compactJson, parseJson}
import org.json4s.scalaz.JsonScalaz._

object Jwt extends App {

  import ClaimsTypes._
  import ClaimsRead._

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
      |  "http://example.com/is_root": true
      |}
    """.stripMargin)


  val cl1: Claim = Coproduct.apply[Claim](Claim.Iss("asd"))
  val cl2: Claim = claim(Claim.Iss("asd"))

  val claims: List[Claim] = List(claim(Claim.Iss("hello")))
  val iss = claims.head.select[Claim.Iss]
  println(iss)


  val x = claimsJson.read[List[Claim]]

  println(x)

}

