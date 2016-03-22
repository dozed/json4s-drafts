package jwtExample

import shapeless.Coproduct

object JwtExample3 extends App {

  import JwtTypes._
  import Jws._

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

  val secret = "thequickbrownfoxjumpsoverthelazydog"

  // encode a token as a JWS
  println(encodeAsJWS(token, secret))

  // generate a MAC for a token
  println(signature(token, secret))

}

