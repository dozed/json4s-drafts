object NewTypeExample extends App {

  import drafts.ReadExt._
  import org.json4s._
  import org.json4s.jackson.parseJson
  import org.json4s.scalaz.JsonScalaz._
  import shapeless._
  import newtype._

  import _root_.scalaz._
  import Scalaz._

  type Email = Newtype[String, EmailOps]
  def Email(s : String) : Email = newtype(s)

  case class EmailOps(s: String) {
    def value: String = s
    def size: Int = s.length
    def hostname: String = s.substring(s.indexOf("@")+1)
  }

  implicit val mkEmailOps = EmailOps

  val orderDoc =
    """
      |{
      |  "orderId": "2120020",
      |  "contact": {
      |    "email": "mail@example.org",
      |    "phone": [ "+2398 2938092", "+2398 2938001" ]
      |  }
      |}
    """.stripMargin

  val orderJson = parseJson(orderDoc)


  implicit val readEmail: JSONR[Email] = readE[Email] { json =>
    for {
      str <- json.read[String]
      _ <- if (!str.contains("@")) UncategorizedError("email", "invalid email format", Nil).left else str.right
    } yield Email(str)
  }


  val x1: Error \/ Email = (orderJson \ "orderId").read[Email]
  // -\/(UncategorizedError(email,invalid email format,List()))

  val x2: Error \/ Email = (orderJson \ "contact" \ "email").read[Email]
  // \/-(mail@example.org)

  println(x2.map(_.hostname))
  // \/-(example.org)


}
