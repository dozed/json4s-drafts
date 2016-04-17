

Drafts for [json4s-scalaz](https://github.com/json4s/json4s/tree/3.4/scalaz): [code](https://github.com/dozed/json4s-drafts/tree/master/src/main/scala/org/json4s/ext/scalaz) & [examples](https://github.com/dozed/json4s-drafts/tree/master/src/test/scala)

Adds:

  - Typeclass instances
    - `Contravariant[JSONW]` to re-use existing writers
    - `Monad[JSONR]` for applicative, functor and monadic parsing
    - `JSON[B].xmap[B](f1: A => B, f2: B => A): JSON[B]`
    - `JSON[B].exmap[B](f1: A => Result[B], f2: B => A): JSON[B]`
  - Methods to define `JSON[A]`/`JSONR[A]`/`JSONW[A]`
    - `JSON.apply[A:JSON](implicit jsonA: JSON[A]): JSON[A]`                (manifest `JSON[A]` from instance)
    - `JSONR.apply[A:JSONL]: JSONL[A]`                                      (implicit lookup)
    - `JSONW.apply[A:JSONW]: JSONW[A]`
    - `JSON.instance[A](f: JValue => Result[A], g: A => JValue): JSON[A]`   (`JSON[A]` from functions)
    - `JSONR.instance[A](f: JValue => Result[A]): JSONR[A]`     (applicative parsing)
    - `JSONR.instanceE[A](f: JValue => Error \/ A): JSONR[A]`   (monadic parsing)
    - `JSONW.instance(f: A => JValue): JSONW[A]`               (write json)
    - `JSONW.withContext[C, A](f: (C, A) => JValue): JSONW[JSONWContext[C, A]]`   (context-dependent JSON writer)
  - JSON object codecs with fixed arity
    - `json2[R, A, B]`
    - `json3[R, A, B, C]`
    - `json4[R, A, B, C, D]`
  - `JSON[A]` for shapeless types
    - HList, Coproduct and NewType
    - Labelled Generic HList (fields of a case class, product)
    - Labelled Generic Coproduct (inhabitants of a type, sum)
    - `deriveJSON[A]`, `deriveJSONR[A]`, `deriveJSONW[A]`
  - Typeclass-based DSL to construct JSON
  - Things
    - [Context dependent writers](https://github.com/dozed/json4s-drafts/blob/master/src/test/scala/ContextDependentWriterExample.scala)
    - [Coproduct JSON encodings & JValue transformations](https://github.com/dozed/json4s-drafts/blob/master/src/test/scala/coproductEncodingsExample.scala)



## Codec for JObject using keys for 1-step traversal and existing `JSON[A]` and `JSON[B]`

```scala
implicit val localDateJson: JSON[LocalDate] = json3[LocalDate, Int, Int, Int]("year", "month", "day")(
  (d: LocalDate) => (d.getYear, d.getMonthOfYear, d.getDayOfMonth).some,
  (year, month, day) => new LocalDate(year, month, day)
)
```


## Derive `JSON[B]` from `JSON[A]` with `xmap`

Assuming there is a `JSON[Long]`, one can derive a `JSON[Instant]` straightfowardly with `xmap`:

```scala
implicit val instantJson = JSON[Long].xmap[Instant](x => new Instant(x), i => i.getMillis)
```


## Writing JSON with `JSONW[A]`


### Using a small DSL

```scala
import org.json4s.ext.scalaz.JsonScalaz._

case class Contact(email: String, phone: List[String])
case class Item(label: String, amount: Double, price: Double)
case class Order(orderId: String, contact: Contact, items: List[Item])

implicit val contactWriter: JSONW[Contact] = JSON.write[Contact](c => {
  ("email" -> c.email) ~
    ("phone" -> c.phone)
})

implicit val itemWriter: JSONW[Item] = JSON.write[Item](i => {
  ("label" -> i.label) ~
    ("amount" -> i.amount) ~
    ("price" -> i.price)
})

implicit val orderWriter: JSONW[Order] = JSON.write[Order](o => {
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

order.toJson
```


### JSONW is contravariant

```scala
sealed trait Algorithm

object Algorithm {
  case object HS256 extends Algorithm
  case object HS384 extends Algorithm
  case object HS512 extends Algorithm
  case object NONE extends Algorithm
}

// JSONW[String] + (Algorithm => String) => JSONW[Algorithm] 
implicit val algorithmWrite = JSONW[String].contramap[Algorithm] {
  case Algorithm.HS256 => "HS256"
  case Algorithm.HS384 => "HS384"
  case Algorithm.HS512 => "HS512"
  case Algorithm.NONE => "NONE"
}

val json = (Algorithm.HS512:Algorithm).toJson
println(json)
// JString(HS512)
```



## Reading JSON with `JSONR[A]`

Methods to define a JSON reader

  - `read[A](f: JValue => Result[A]): JSONR[A]`
  - `readE[A](f: JValue => Error \/ A): JSONR[A]`
  - `JSONR.apply[A]: JSONR[A]`


### Applicative parsing with `Validation`

Characteristics

  - Runs all validations
  - All errors are accumulated
  - Applicative syntax

 
```scala
implicit val intervalRead: JSONR[Interval] = JSONR.instance[Interval] { json =>
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
implicit val itemRead: JSONR[Item] = JSONR.instanceE[Item]({ json =>
  for {
    label <- (json \ "label").read[String]
    amount <- (json \ "amount" \ "value").read[Int]
    price <- (json \ "price" \ "value").read[Double]
  } yield Item(label, amount, price)
})
```


### JSONR is a Functor

```scala
implicit val durationRead: JSONR[Duration] = JSONR[Int] map (x => new Duration(x))
implicit val instantRead: JSONR[Instant] = JSONR[Int] map (x => new Instant(x))
```


### emap

```scala
implicit val algorithmRead: JSONR[Algorithm] = JSONR[String].map(_.toUpperCase).emap {
  case "HS256" => Algorithm.HS256.successNel
  case "HS384" => Algorithm.HS384.successNel
  case "HS512" => Algorithm.HS512.successNel
  case "NONE" => Algorithm.NONE.successNel
  case x => Fail.apply("", "one of: HS256, HS384, HS512, NONE", List(x))
}

println(json.read[Algorithm].require)
```


## Deriving `JSONR[A]` and `JSONW[A]` instances for generic representations of types

For the generic representations labelled HList and labelled Coproduct instance can be derived.
A labelled HList represents the fields of a class. A labelled Coproduct represents the inhabitants of a type.
There are three methods: `deriveJSON[A]`, `deriveJSONR[A]`, `deriveJSONW[A]`  


```scala
import org.json4s.ext.scalaz.JsonScalaz._

sealed trait Measure
case class Gram(value: Double) extends Measure
case class Teaspoon(value: Double) extends Measure
case class Tablespoon(value: Double) extends Measure
case class Handful(value: Double) extends Measure
case class Pieces(value: Double) extends Measure
case class Milliliter(value: Double) extends Measure

implicit def queryParamJSON = deriveJSON[QueryParam]
implicit def cookieJSON     = deriveJSON[Cookie]
implicit def requestJSON    = deriveJSON[Request]
implicit def responseJSON   = deriveJSON[Response]
implicit def entryJSON      = deriveJSON[Entry]


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

println(gram(50).toJson.read[Measure])
// \/-(Gram(50.0))
```


For automatic derivation without defining the implicit values it is also possible to import `import org.json4s.ext.scalaz.JsonScalaz.auto._`.
Beware, this can lead to unexpected compilation successes.
