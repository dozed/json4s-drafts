
object ParseExample extends App {

  import org.json4s._
  import org.json4s.jackson.parseJson
  import org.json4s.scalaz.JsonScalaz._
  import _root_.scalaz._, Scalaz._
  import drafts.ReadExt._

  implicitly[JSONR[String]]
  implicitly[JSONR[List[String]]]

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
      |       "amount": { "value": 200 },
      |       "price": { "value": 1.99 }
      |     },
      |     {
      |       "label": "bar item",
      |       "amount": { "value": 100 },
      |       "price": { "value": 2.50 }
      |     }
      |  ]
      |}
    """.stripMargin

  val orderJson = parseJson(orderDoc)


  case class Contact(email: String, phone: List[String])
  case class Item(label: String, amount: Int, price: Double)
  case class Order(id: String, contact: Contact, items: List[Item])



  implicit val contactRead2: JSONR[Contact] = read[Contact] { json =>
    for {
      email <- (json \ "email").readAs[String]
      phone <- (json \ "phone").readAs[List[String]]
    } yield Contact(email, phone)
  }

  implicit val itemReadM: JSONR[Item] = read[Item]({ json =>
    for {
      label <- (json \ "label").readAs[String]
      amount <- (json \ "amount" \ "value").readAs[Int]
      price <- (json \ "price" \ "value").readAs[Double]
    } yield Item(label, amount, price)
  })

  implicit val orderRead = read[Order] { json =>
    for {
      id <- (json \ "orderId").readAs[String]
      contact <- (json \ "contact").readAs[Contact]
      items <- (json \ "items").readAs[List[Item]]
    } yield Order(id, contact, items)
  }


  println((orderJson \ "contact").readAs[Contact])
  // \/-(Contact(mail@example.org,List(+2398 2938092, +2398 2938001)))

  println(orderJson.validate[Order])
  // Success(Order(2120020,Contact(mail@example.org,List(+2398 2938092, +2398 2938001)),List(Item(foo item,200,1.99), Item(bar item,100,2.5))))

  println(orderJson.readAs[Order])
  // \/-(Order(2120020,Contact(mail@example.org,List(+2398 2938092, +2398 2938001)),List(Item(foo item,200,1.99), Item(bar item,100,2.5))))

}
