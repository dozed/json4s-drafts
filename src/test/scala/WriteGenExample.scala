import drafts.Macros
import org.json4s.ext.scalaz.JsonScalaz._

object WriteGenExample extends App {

  case class Contact(email: String, phone: List[String])
  case class Item(label: String, amount: Double, price: Double)
  case class Order(orderId: String, contact: Contact, items: List[Item])



  implicit val writer2: JSONW[Contact] = Macros.writerGen[Contact]


  println(Contact("foo@example.org", List("+123", "+1293")).toJson)
  // JObject(List((email,JString(foo@example.org)), (phone,JArray(List(JString(+123), JString(+1293))))))


}
