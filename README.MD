

Drafts for [json4s-scalaz](https://github.com/json4s/json4s/tree/3.4/scalaz): [code](https://github.com/dozed/json4s-drafts/tree/master/src/main/scala/org/json4s/ext/scalaz) & [examples](https://github.com/dozed/json4s-drafts/tree/master/src/test/scala)

Adds:

  - Typeclass-based DSL to construct JSON
  - Methods to define JSONR[A]/JSONW[A]
    - JSON.read[A](f: JValue => Result[A]): JSONR[A]     (applicative parsing)
    - JSON.readE[A](f: JValue => Error \/ A): JSONR[A]   (monadic parsing)
    - JSON.readL[A:JSONL]: JSONL[A]                      (implicit lookup)
    - JSON.writeL[A:JSONW]: JSONW[A]
    - JSON.write(f: A => JValue): JSONW[A]               (write json)
    - JSON.write.context[C, A](f: (C, A) => JValue): JSONW[JSONWContext[C, A]]   (context-dependent JSON writer)
  - Typeclass instances
    - Contravariant[JSONW] to re-use existing writers
    - Monad[JSONR] for applicative, functor and monadic parsing
  - JSONR[A]/JSONW[A] for shapeless HList, CoProduct and NewType
  - JSONR[A]/JSONW[A] TypeClass derivation for LabelledGeneric (product and coroduct)


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



## Automatically deriving `JSONR[A]` and `JSONW[A]`


Import `import org.json4s.ext.scalaz.JsonScalaz.auto._`

```scala
import org.json4s.ext.scalaz.JsonScalaz._
import org.json4s.ext.scalaz.JsonScalaz.auto._

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
```