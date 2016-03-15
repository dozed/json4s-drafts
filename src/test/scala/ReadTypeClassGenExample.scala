
object ReadTypeClassGenExample extends App {

  import org.json4s._
  import org.json4s.jackson.parseJson
  import org.json4s.scalaz.JsonScalaz._
  import _root_.scalaz._, Scalaz._
  import drafts.ReadExt._
  import drafts.ReadExt.JSONRExt
  import drafts.ReadExt.JSONRExt._

  val orderDoc =
    """
      |{
      |  "orderId": "2120020",
      |  "contact": {
      |    "email": "mail@example.org",
      |    "phone": [ "+2398 2938092", "+2398 2938001" ]
      |  },
      |  "items": [
      |     {
      |       "label": "foo item",
      |       "amount": 200,
      |       "price": 1.99
      |     },
      |     {
      |       "label": "bar item",
      |       "amount": 100,
      |       "price": 2.50
      |     }
      |  ]
      |}
    """.stripMargin

  val orderJson = parseJson(orderDoc)


  case class Contact(email: String, phone: List[String])
  case class Item(label: String, amount: Int, price: Double)
  case class Order(orderId: String, contact: Contact, items: List[Item])


  println(orderJson.validate[Order])
  // Success(Order(2120020,Contact(mail@example.org,List(+2398 2938092, +2398 2938001)),List(Item(foo item,200,1.99), Item(bar item,100,2.5))))

  println(orderJson.read[Order])
  // \/-(Order(2120020,Contact(mail@example.org,List(+2398 2938092, +2398 2938001)),List(Item(foo item,200,1.99), Item(bar item,100,2.5))))

}


