import org.json4s._
import org.json4s.jackson.{prettyJson, parseJson}
import org.json4s.ext.scalaz.JsonScalaz._

object Model {

  sealed trait Measure
  case class Gram(value: Double) extends Measure
  case class Teaspoon(value: Double) extends Measure
  case class Tablespoon(value: Double) extends Measure
  case class Handful(value: Double) extends Measure
  case class Pieces(value: Double) extends Measure
  case class Milliliter(value: Double) extends Measure

  def gram(value: Double): Measure = Gram(value)
  def teaspoon(value: Double): Measure = Teaspoon(value)
  def tablespoon(value: Double): Measure = Tablespoon(value)
  def handful(value: Double): Measure = Handful(value)
  def pieces(value: Double): Measure = Pieces(value)
  def milliliter(value: Double): Measure = Milliliter(value)

}

object CoproductExample1 extends App {

  // coproduct is defined in a different scope due to https://issues.scala-lang.org/browse/SI-7046
  import Model._

  implicit def measureJSON = deriveJSON[Measure]

  val xs: List[Measure] = List(Gram(10.0), Teaspoon(3))

  val jsonOk = parseJson(
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

  val jsonKo = parseJson(
    """
      |[{
      |  "Teasoon": {
      |    "value": 2.0
      |  }
      |}]
    """.stripMargin
  )

  println(gram(50).toJson)
  // JObject(List((Gram,JObject(List((value,JDouble(50.0)))))))

  println(prettyJson(gram(50).toJson))
  //  {
  //    "Gram" : {
  //      "value" : 50.0
  //    }
  //  }

  println((Gram(50):Measure).toJson)
  // JObject(List((value,JDouble(50.0))))

  println(gram(50).toJson.read[Measure])
  // \/-(Gram(50.0))


  println(jsonOk.read[List[Measure]])
  // \/-(List(Gram(10.0), Teaspoon(2.0)))

  println(jsonKo.read[List[Measure]])
  // -\/(UncategorizedError(invalid_json_for_coproduct,no element of this coproduct matched the json,List()))

}
