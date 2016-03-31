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

}
