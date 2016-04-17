import org.json4s._
import org.json4s.ext.scalaz.JsonScalaz._
import org.json4s.jackson.parseJson
import scalaz._, Scalaz._

object ReadExample extends App {


  implicitly[JSONR[String]]
  implicitly[JSONR[List[String]]]


  JNothing.read[String]
  // -\/(UnexpectedJSONError(JNothing,class org.json4s.JsonAST$JString))


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



  implicit val contactRead2 = JSONR.instanceE[Contact] { json =>
    for {
      email <- (json \ "email").read[String]
      phone <- (json \ "phone").read[List[String]]
    } yield Contact(email, phone)
  }

  implicit val itemReadM = JSONR.instanceE[Item]({ json =>
    for {
      label <- (json \ "label").read[String]
      amount <- (json \ "amount" \ "value").read[Int]
      price <- (json \ "price" \ "value").read[Double]
    } yield Item(label, amount, price)
  })

  implicit val orderRead = JSONR.instanceE[Order] { json =>
    for {
      id <- (json \ "orderId").read[String]
      contact <- (json \ "contact").read[Contact]
      items <- (json \ "items").read[List[Item]]
    } yield Order(id, contact, items)
  }


  println((orderJson \ "contact").read[Contact])
  // \/-(Contact(mail@example.org,List(+2398 2938092, +2398 2938001)))

  println(orderJson.validate[Order])
  // Success(Order(2120020,Contact(mail@example.org,List(+2398 2938092, +2398 2938001)),List(Item(foo item,200,1.99), Item(bar item,100,2.5))))

  println(orderJson.read[Order])
  // \/-(Order(2120020,Contact(mail@example.org,List(+2398 2938092, +2398 2938001)),List(Item(foo item,200,1.99), Item(bar item,100,2.5))))

}
