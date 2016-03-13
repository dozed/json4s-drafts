

object coproductWriteExample extends App {

  import drafts.WriteExt._
  import drafts.WriteExt.JSONWExt
  import drafts.WriteExt.JSONWExt._
  import org.json4s.scalaz.JsonScalaz._


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


  object Measure {
    implicit val instance = JSONWExt[Measure]
  }

  // val tag: Measure => String = typeTagGen[Measure]

  println(gram(50).toJson)
  // JObject(List((value,JDouble(50.0))))

  println(Gram(50).toJson)



}
