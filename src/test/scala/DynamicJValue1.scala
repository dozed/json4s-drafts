
object DynamicJValue1 extends App {

  import org.json4s._
  import org.json4s.jackson.{parseJson, compactJson}
  import org.json4s.ext.scalaz.JsonScalaz._
  import scalaz._, Scalaz._

  val text =
    """
      |{
      |  "s" : "string",
      |  "i" : 123,
      |  "dyn" : { "ss": "another string" }
      |}
    """.stripMargin

  case class Boo(s: String, i: Int, dyn: DynamicJValue)

  object Boo {
    implicit val booJson = deriveJSON[Boo]
  }

  implicit val dynamicJValueJson = JSON.json[DynamicJValue](
    json => DynamicJValue.dyn(json).successNel,
    _.raw
  )

  val json = parseJson(text)


  json.read[Boo] map { boo =>

    println(boo.dyn.ss.raw)
    // JString(another string)

    println(compactJson(boo.toJson))
    // {"s":"string","i":123,"dyn":{"ss":"another string"}}

  }

}
