package jwtExample

import org.json4s._
import org.json4s.ext.scalaz.JsonScalaz._
import org.json4s.jackson.{parseJson, prettyJson}
import shapeless.Coproduct
import scalaz._, Scalaz._

object JwtExample2 extends App {

  import JwtJSON._
  import JwtTypes._

  val token = Jwt(
    List(
      Coproduct[Header](Algorithm.HS256: Algorithm),
      Coproduct[Header](Header.Typ("JWT"))
    ),
    List(
      Coproduct[Claim](Claim.Iss("joe")),
      Coproduct[Claim](Claim.Exp(1300819380)),
      Coproduct[Claim](Claim.Custom("http://example.com/is_root", "true")),
      Coproduct[Claim](Claim.Custom("view", "[100, 200, 300]")),
      Coproduct[Claim](Claim.Custom("admin", "[100, 200, 300]")),
      Coproduct[Claim](Claim.Aud(Coproduct[StringOrList](List("foo", "bar")))),
      Coproduct[Claim](Claim.Aud(Coproduct[StringOrList]("foo")))
    )
  )

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


  val headerJson2 = token.headers.toJson
  val claimJson2 = token.claims.toJson

  println(prettyJson(token.headers.toJson))
  println(prettyJson(token.claims.toJson))

  println(headerJson === headerJson2)
  println(claimJson === claimJson2)


}

