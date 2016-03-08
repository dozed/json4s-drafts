
object ValidationExample extends App {

  import drafts.ReadExt._
  import org.json4s._
  import org.json4s.jackson.parseJson
  import org.json4s.scalaz.JsonScalaz._
  import shapeless._
  import newtype._

  import _root_.scalaz._, Scalaz._

  type PhoneNumber = Newtype[String, PhoneNumberOps]
  def PhoneNumber(s: String) : PhoneNumber = newtype(s)

  case class PhoneNumberOps(s: String) {
    def value: String = s
    def size: Int = s.length
    def suffix: String = s.substring(s.indexOf(" ")+1)
  }

  implicit val mkPhoneNumberOps = PhoneNumberOps

  val orderDoc =
    """
      |{
      |  "orderId": "x2120020",
      |  "contact": {
      |    "email": "mail@example.org",
      |    "phone": [ "+2398 2938092", "+2398 2938001" ]
      |  }
      |}
    """.stripMargin

  val orderJson = parseJson(orderDoc)

  val isInternational: String => Result[String] = x => if (x.contains("+")) x.successNel else Fail("", "is not an international number")
  def matchesRegex(regex: String): String => Result[String] = x => if (x.matches(regex)) x.successNel else Fail("", s"does not match regex $regex")
  val phoneNumberWhitelist = matchesRegex("""[0-9\s\+]+""")



  implicit val phoneNumberRead: JSONR[PhoneNumber] = jsonr[PhoneNumber] { json =>
    json.validate[String] flatMap { s =>
      isInternational(s) append phoneNumberWhitelist(s)
    } map (str => PhoneNumber(str))
  }



  val x1: Error \/ PhoneNumber = (orderJson \ "orderId").read[PhoneNumber]
  // -\/(UncategorizedError(,is not an international number,List()))

  val x2: ValidationNel[Error, PhoneNumber] = (orderJson \ "orderId").validate[PhoneNumber]
  // Failure(NonEmptyList(UncategorizedError(,is not an international number,List()), UncategorizedError(,does not match regex [0-9\s\+]+,List())))

  val x3: Error \/ PhoneNumber = (orderJson \ "contact" \ "phone")(0).read[PhoneNumber]
  // \/-(+2398 2938092+2398 2938092)


  println(x1)
  println(x2)
  println(x3)


}
