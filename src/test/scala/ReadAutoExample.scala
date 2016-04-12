import org.json4s._
import org.json4s.jackson.parseJson
import org.json4s.ext.scalaz.JsonScalaz._
import shapeless.LabelledGeneric

import shapeless._
import scalaz._
import scalaz.syntax.functor._

object ReadAutoExample extends App {


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

  implicit val itemJSON = JSON.derive[Item]
  implicit val contactJSON = JSON.derive[Contact]
  implicit val orderJSON = JSON.derive[Order]


  println(orderJson.validate[Order])
  // Success(Order(2120020,Contact(mail@example.org,List(+2398 2938092, +2398 2938001)),List(Item(foo item,200,1.99), Item(bar item,100,2.5))))

  println(orderJson.read[Order])
  // \/-(Order(2120020,Contact(mail@example.org,List(+2398 2938092, +2398 2938001)),List(Item(foo item,200,1.99), Item(bar item,100,2.5))))


  val orderJsonKo = parseJson(
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
      |       "amount": 200
      |     },
      |     {
      |       "label": "bar item",
      |       "amount": 100,
      |       "price": 2.50
      |     }
      |  ]
      |}
    """.stripMargin)

  // TODO improve error reporting and handling

  println(orderJsonKo.validate[Order])
  // Failure(NonEmptyList(UnexpectedJSONError(JNothing,class org.json4s.JsonAST$JDouble)))





}


