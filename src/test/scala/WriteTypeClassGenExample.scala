

object WriteTypeClassGenExample extends App {

  import drafts.WriteExt._
  import drafts.WriteExt.JSONWExt
  import drafts.WriteExt.JSONWExt._
  import org.json4s._
  import org.json4s.scalaz.JsonScalaz._

  import _root_.scalaz._, Scalaz._


  sealed trait Measure
  case class Gram(value: Double) extends Measure
  case class Teaspoon(value: Double) extends Measure
  case class Tablespoon(value: Double) extends Measure
  case class Handful(value: Double) extends Measure
  case class Pieces(value: Double) extends Measure
  case class Milliliter(value: Double) extends Measure



  case class Contact(email: String, phone: List[String])
  case class Item(label: String, amount: Double, price: Double)
  case class Order(orderId: String, contact: Contact, items: List[Item])

  val tag: Measure => String = typeTagGen[Measure]

  println(Gram(50).toJson)
  // JObject(List((value,JDouble(50.0))))

  // println(Teaspoon(50).toJson)

  println(Contact("foo@example.org", List("+123", "+1293")).toJson)
  // JObject(List((email,JString(foo@example.org)), (phone,JArray(List(JString(+123), JString(+1293))))))


}
