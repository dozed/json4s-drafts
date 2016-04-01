import org.json4s._
import org.json4s.jackson.{parseJson, compactJson}
import org.json4s.ext.scalaz.JsonScalaz._

import shapeless._
import scalaz._, Scalaz._

object HlistExample extends App {

  implicitly[JSONR[String]]
  implicitly[JSONR[List[String]]]
  implicitly[JSONR[String :: Int :: HNil]]
  val json = parseJson("""["hey", 42]""")

  val x = json.read[String :: Int :: HNil]

  println(x)
  // \/-(hey :: 42 :: HNil)

  implicitly[JSONW[String]]
  implicitly[JSONW[List[String]]]
  implicitly[JSONW[String :: Int :: HNil]]


  val x2 = ("hey" :: 42 :: List("foo", "bar") :: HNil).toJson

  println(compactJson(x2))
  // ["hey",42,["foo","bar"]]

  case class Foo(x: Int)


  // there is no JSONR[Foo] in implicit scope, therefore it does not compile (illTyped)
  shapeless.test.illTyped(
    """("hey" :: Foo(42) :: HNil).toJson.read[String :: Foo :: HNil]"""
  )

  // import JsonScalaz._ for automatic derivation of JSONR/JSONW typeclass instances
  // import org.json4s.ext.scalaz.JsonScalaz.auto._


}
