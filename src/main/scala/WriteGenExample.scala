

object WriteGenExample extends App {

  import drafts.WriteExt._
  import org.json4s.scalaz.JsonScalaz._


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

  implicit val writer: JSONW[Gram] = writerGen[Gram]
  implicit val writer2: JSONW[Contact] = writerGen[Contact]


  println(Gram(50))
  println(Contact("foo@example.org", List("+123", "+1293")))


}
