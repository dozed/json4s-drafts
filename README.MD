

Drafts for [json4s-scalaz](https://github.com/json4s/json4s/tree/3.4/scalaz): [code](https://github.com/dozed/json4s-drafts/tree/master/src/main/scala/org/json4s/ext/scalaz) & [examples](https://github.com/dozed/json4s-drafts/tree/master/src/test/scala)

Adds:

  - Typeclass-based DSL to write JSON
  - Methods to define JSON readers
  - Support for shapeless NewType and TypeClass derivation


## Writing JSON with `JSONW[A]`


### Using a small DSL

```scala
import org.json4s.ext.scalaz.JsonScalaz._

case class Contact(email: String, phone: List[String])
case class Item(label: String, amount: Double, price: Double)
case class Order(orderId: String, contact: Contact, items: List[Item])

implicit val contactWriter: JSONW[Contact] = write[Contact](c => {
  ("email" -> c.email) ~
    ("phone" -> c.phone)
})

implicit val itemWriter: JSONW[Item] = write[Item](i => {
  ("label" -> i.label) ~
    ("amount" -> i.amount) ~
    ("price" -> i.price)
})

implicit val orderWriter: JSONW[Order] = write[Order](o => {
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
```


## Reading JSON with `JSONR[A]`

Methods to define a JSON reader

  - `read[A](f: JValue => Result[A]): JSONR[A]`
  - `readE[A](f: JValue => Error \/ A): JSONR[A]`
  - `readL[A]: JSONR[A]`


### Applicative parsing with `Validation`

Characteristics

  - Runs all validations
  - All errors are accumulated
  - Applicative syntax

 
```scala
implicit val intervalRead: JSONR[Interval] = read[Interval] { json =>
  (
    (json \ "start").validate[Long] |@|
    (json \ "end").validate[Long]
  ).apply {
    case (start, end) => new Interval(start, end)
  }
}
```


### Monadic parsing with `\/`

Characteristics

  - Fail fast: parses until the first error
  - Does not accumulate errors
  - Monadic syntax


```scala
implicit val itemRead: JSONR[Item] = readE[Item]({ json =>
  for {
    label <- (json \ "label").read[String]
    amount <- (json \ "amount" \ "value").read[Int]
    price <- (json \ "price" \ "value").read[Double]
  } yield Item(label, amount, price)
})
```


### JSONR is a Functor

```scala
implicit val durationRead: JSONR[Duration] = readL[Int] map (x => new Duration(x))
implicit val instantRead: JSONR[Instant] = readL[Int] map (x => new Instant(x))
```


