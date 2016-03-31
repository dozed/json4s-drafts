import org.json4s.jackson.prettyJson

import org.json4s.ext.scalaz.JsonScalaz._
import org.json4s.ext.scalaz.JsonScalaz.auto._

object coproductWriteExample extends App {

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

  println(gram(50).toJson)
  // JObject(List((Gram,JObject(List((value,JDouble(50.0)))))))

  println(prettyJson(gram(50).toJson))
  //  {
  //    "Gram" : {
  //      "value" : 50.0
  //    }
  //  }

  println(Gram(50).toJson)
  // JObject(List((value,JDouble(50.0))))

  // TODO fix reading
  println(gram(50).toJson.read[Measure])
  // JObject(List((value,JDouble(50.0))))
  // Exception in thread "main" java.lang.RuntimeException: no _tpe flag, not possible to discriminate types


}
