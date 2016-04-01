import org.json4s.jackson.prettyJson

import org.json4s.ext.scalaz.JsonScalaz._

object coproductWriteExample extends App {

  sealed trait Measure
  case class Gram(value: Double) extends Measure
  case class Teaspoon(value: Double) extends Measure
  case class Tablespoon(value: Double) extends Measure
  case class Handful(value: Double) extends Measure
  case class Pieces(value: Double) extends Measure
  case class Milliliter(value: Double) extends Measure

  object Measure {
    import org.json4s.ext.scalaz.JsonScalaz.auto._
    implicit val json = JSON.json[Measure]
  }

  def gram(value: Double): Measure = Gram(value)
  def teaspoon(value: Double): Measure = Teaspoon(value)
  def tablespoon(value: Double): Measure = Tablespoon(value)
  def handful(value: Double): Measure = Handful(value)
  def pieces(value: Double): Measure = Pieces(value)
  def milliliter(value: Double): Measure = Milliliter(value)

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


}
