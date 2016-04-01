package jwtExample

import shapeless.newtype._
import shapeless.{:+:, Coproduct, _}

import org.json4s._
import org.json4s.ext.scalaz.JsonScalaz._
import org.json4s.jackson.{compactJson, prettyJson, parseJson}

import scalaz._, Scalaz._

object JwtExample1 extends App {

  import JwtTypes._
  import JwtJSON._

  val headerJson = parseJson(
    """
      |{
      |  "typ": "JWT",
      |  "alg": "HS256"
      |}
    """.stripMargin)


  val claimJson = parseJson(
    """
      |{
      |  "iss": "joe",
      |  "exp": 1300819380,
      |  "http://example.com/is_root": true,
      |  "view": [100,200,300],
      |  "admin": [100,200,300],
      |  "aud": ["foo", "bar"],
      |  "aud": "foo"
      |}
    """.stripMargin)


  val cl1: Claim = Coproduct.apply[Claim](Claim.Iss("asd"))
  val cl2: Claim = claim(Claim.Iss("asd"))

  val claims: List[Claim] = List(claim(Claim.Iss("hello")))
  val iss = claims.head.select[Claim.Iss]
  println(iss)


  val x1 = claimJson.read[List[Claim]]
  println(x1)

  // TODO compiler hangs
  // x.toJson

  println(prettyJson((x1.require).toJson))


  val x2 = headerJson.read[List[Header]]

  println(x2)
  println(prettyJson((x2.require).toJson))

}

