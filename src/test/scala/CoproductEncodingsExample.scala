
import org.json4s._
import org.json4s.ext.scalaz.JsonScalaz._
import org.json4s.jackson.{parseJson, prettyJson}

import scalaz._
import syntax.validation._
import syntax.traverse._
import syntax.equal._
import std.list._
import std.string._
import syntax.std.string._
import syntax.std.option._

object CoproductEncodingsExample extends App {

  // coproduct encoding
  val json1 = parseJson(
    """
      |[{
      |  "Gram": {
      |    "value": 10.0
      |  }
      |}, {
      |  "Teaspoon": {
      |    "value": 2.0
      |  }
      |}]
    """.stripMargin
  )

  // flat coproduct encoding
  val json2 = parseJson(
    """
      |[{
      |  "type": "Gram",
      |  "value": 10.0
      |}, {
      |  "type": "Teaspoon",
      |  "value": 2.0
      |}]
    """.stripMargin
  )



  val nestedEncodingToFlatEncoding = jsonTransform { json =>
    for {
      jobj <- json.validate[JObject]
      x <- jobj.obj.headOption.toSuccessNel(UncategorizedError("no_type_tag", "JSON object has no type tag", List(jobj)):Error)
      (key, value) = x
    } yield {
      ("type", JString(key)) ~ value
    }
  }

  val flatEncodingToNestedEncoding = jsonTransform { json =>
    for {
      jobj <- json.validate[JObject]
      key <- (jobj \ "type").validate[String] ||| Fail("no_type_tag", "JSON object has no type tag", List(jobj))
      fields = jobj.filterField { case (k,v) => k =/= "type" }
    } yield {
      JObject(key -> JObject(fields))
    }
  }

  val json3 = json1.validate[JArray].flatMap { jarr =>

    jarr.children.map(nestedEncodingToFlatEncoding).sequence[Result, JValue] map JArray.apply

  }.require

  println(prettyJson(json3))


  val json4 = json3.validate[JArray].flatMap { jarr =>

    jarr.children.map(flatEncodingToNestedEncoding).sequence[Result, JValue] map JArray.apply

  }.require

  println(prettyJson(json4))



  println(json1 == json4)
  println(json2 == json3)


}
