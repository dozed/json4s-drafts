import scalaz._, Scalaz._

object coproductReadExample extends App {

  import drafts.ReadExt._
  import drafts.ReadExt.JSONRExt
  import drafts.ReadExt.JSONRExt._
  import org.json4s._
  import org.json4s.scalaz.JsonScalaz._
  import org.json4s.jackson.parseJson


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


  val xs: List[Measure] = List(Gram(10.0), Teaspoon(3))

  val json = parseJson(
    """
      |[{
      |  "_tpe": "Gram",
      |  "value": 10.0
      |}, {
      |  "_tpe": "Teason",
      |  "value": 2.0
      |}]
    """.stripMargin
  )


  println(json.read[List[Measure]])

}
