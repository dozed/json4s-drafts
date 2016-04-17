import org.json4s.jackson._
import org.json4s.ext.scalaz.JsonScalaz._

object WriteExample extends App {


  case class Contact(email: String, phone: List[String])
  case class Item(label: String, amount: Double, price: Double)
  case class Order(orderId: String, contact: Contact, items: List[Item])

  implicit val contactWriter = JSONW.instance[Contact](c => {
    ("email" -> c.email) ~
      ("phone" -> c.phone)
  })

  implicit val itemWriter = JSONW.instance[Item](i => {
    ("label" -> i.label) ~
      ("amount" -> i.amount) ~
      ("price" -> i.price)
  })

  implicit val orderWriter = JSONW.instance[Order](o => {
    ("orderId" -> o.orderId) ~
      ("contact" -> o.contact) ~
      ("items" -> o.items)
  })


  val order = Order(
    "2120020",
    Contact("mail@example.org", List("+2398 2938092", "+2398 2938001")),
    List(
      Item("foo item", 200, 1.99),
      Item("bar item", 100, 2.50)
    )
  )




  val orderJson = order.toJson

  println(compactJson(orderJson))
  // {"orderId":"2120020","contact":{"email":"mail@example.org","phone":["+2398 2938092","+2398 2938001"]},"items":[{"label":"foo item","amount":200.0,"price":1.99},{"label":"bar item","amount":100.0,"price":2.5}]}



}
