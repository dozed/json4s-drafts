package jwt

import ClientProperties._
import org.json4s.ext.scalaz.JsonScalaz._

import org.specs2.mutable.Specification
import org.specs2.ScalaCheck
import org.specs2.scalaz.ScalazMatchers

import shapeless.{:+:, Coproduct, _}

import scalaz._
import Scalaz._

class JwtSpecs extends Specification with ScalaCheck with ScalazMatchers {

  val cp = isUser("42") |+| isOwner("1000") |+| isViewer("2000")
  val token: Jwt = createClientPropertyToken(cp, "secret", Algorithm.HS512).require
  val str: String = "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzUxMiJ9.eyJjcCI6eyJpc1VzZXIiOiI0MiIsImlzVmlld2VyIjpbIjIwMDAiXSwiaXNPd25lciI6WyIxMDAwIl19fQ==./DbdaD2KrdSGBDv+GBbP5wScA5PviQdCIyFK6mxuWQu/P/2Lv1vNpbh8D+XUtOokbvpyUnKHxhiGHswRiwt/uw=="

  val secret = "thequickbrownfoxjumpsoverthelazydog"

  "A JWT can be written to a String (compact representation)" in {

    token.compact must beEqualTo(str)

  }

  "A JWT can be read from a String (compact representation)" in {

    val token2 = Jwt.decode(str).require

    //    token2 should equal(token2)
    //
    //    val cpStr2 = token2.payload.collect { case Claim.Custom(ClientProperties.KEY_CLIENT_PROPERTY, value) => value }.head
    //    val cp2 = cpStr2.read[ClientProperty].require
    //
    //    cp should equal(cp2)

    failure

  }

  "A JWT can be validated" in {

    val s1 = "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzUxMiJ9.eyJpc3MiOiJpc3MiLCJzdWIiOiJzdWIiLCJhdWQiOiJhdWQiLCJhdWQiOlsiYXVkIiwiYXVkMiJdLCJleHAiOjQyLCJuYmYiOjQyLCJpYXQiOjQyLCJqdGkiOiJqdGkiLCJmb28iOiJiYXIifQ==.9SCDyruJ9p0SGkzGdMdBc6O5wLK1G7MKtGuNABVEUBnyMDI1HNPo3BRAQhxgylA+cmdjEyeq6FTKY62r0sBemw=="
    val s2 = "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzUxMiJ9.eyJpc3MiOiJpc3MiLCJzdWIiOiJzdWIiLCJhdWQiOiJhdWQiLCJhdWQiOlsiYXVkIiwiYXVkMiJdLCJleHAiOjQyLCJuYmYiOjQyLCJpYXQiOjQyLCJqdGkiOiJqdGkiLCJmb28iOiJiYXIifQ==.9SCDyruJ9p0SGkzGdMdBc6O5wLK1G7MKtGuNABVEUBnyMDI1HNPo3BRAQhxgylA+cmdjEyeq6FTKY62r0sBw=="
    val s3 = "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzUxMiJ9eyJpc3MiOiJpc3MiLCJzdWIiOiJzdWIiLCJhdWQiOiJhdWQiLCJhdWQiOlsiYXVkIiwiYXVkMiJdLCJleHAiOjQyLCJuYmYiOjQyLCJpYXQiOjQyLCJqdGkiOiJqdGkiLCJmb28iOiJiYXIifQ==.9SCDyruJ9p0SGkzGdMdBc6O5wLK1G7MKtGuNABVEUBnyMDI1HNPo3BRAQhxgylA+cmdjEyeq6FTKY62r0sBw=="

    Jwt.validate(s1, secret) must be_\/- // \/-(())
    Jwt.validate(s2, secret) must beLeftDisjunction // -\/(InvalidSignature)
    Jwt.validate(s3, secret) must beLeftDisjunction // -\/(InvalidJwsCompact)

  }


  "A JWT can be signed" in {

    val claims =
      List[Claim](
        Claim.Iss("iss"),
        Claim.Sub("sub"),
        Claim.Aud(Coproduct[StringOrList]("aud")),
        Claim.Aud(Coproduct[StringOrList](List("aud", "aud2"))),
        Claim.Exp(42),
        Claim.Nbf(42),
        Claim.Iat(42),
        Claim.Jti("jti"),
        Claim.Custom("foo", "[\"aab\", \"aabba\"]")
      )

    Jwt.sign(claims, "", Algorithm.HS512) must beLeftDisjunction
    Jwt.sign(claims, "foo", Algorithm.NONE) must beLeftDisjunction
    Jwt.sign(claims, "foo", Algorithm.HS512) must beRightDisjunction

  }

  "A JWT can be converted to/from the JSON representation" in {

    val claims =
      List[Claim](
        Claim.Iss("iss"),
        Claim.Sub("sub"),
        Claim.Aud(Coproduct[StringOrList]("aud")),
        Claim.Aud(Coproduct[StringOrList](List("aud", "aud2"))),
        Claim.Exp(42),
        Claim.Nbf(42),
        Claim.Iat(42),
        Claim.Jti("jti"),
        Claim.Custom("foo", "[\"aab\", \"aabba\"]")
      )

    val jws1 = Jwt.sign(claims, secret, Algorithm.HS512).require

    val json = jws1.toJson
    val jws2 = json.read[Jws[List[Claim]]].require

    // jws1 must equal(jws2)

    println(json.nospaces)
    println(jws1)
    println(jws2)

    failure

  }

  "A JWS can be constructed" in {

    val jws1 = Jws[List[Claim]](
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

    success


  }


}
