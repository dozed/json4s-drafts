import org.json4s._
import org.json4s.jackson.parseJson
import org.json4s.ext.scalaz.JsonScalaz._

import scalaz._, Scalaz._

object coproductReadExample extends App {



  sealed trait Measure
  case class Gram(value: Double) extends Measure
  case class Teaspoon(value: Double) extends Measure
  case class Tablespoon(value: Double) extends Measure
  case class Handful(value: Double) extends Measure
  case class Pieces(value: Double) extends Measure
  case class Milliliter(value: Double) extends Measure

  object Measure {
    import org.json4s.ext.scalaz.JsonScalaz.auto._
    implicit val measureJson = JSON.json[Measure]
  }

  def gram(value: Double): Measure = Gram(value)
  def teaspoon(value: Double): Measure = Teaspoon(value)
  def tablespoon(value: Double): Measure = Tablespoon(value)
  def handful(value: Double): Measure = Handful(value)
  def pieces(value: Double): Measure = Pieces(value)
  def milliliter(value: Double): Measure = Milliliter(value)


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


  println(jsonOk.read[List[Measure]])
  // \/-(List(Gram(10.0), Teaspoon(2.0)))

  println(jsonKo.read[List[Measure]])
  // -\/(UncategorizedError(invalid_json_for_coproduct,no element of this coproduct matched the json,List()))

}
