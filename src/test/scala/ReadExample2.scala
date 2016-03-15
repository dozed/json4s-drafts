
object ReadExample2 extends App {

  import org.json4s._
  import drafts.WriteExt._
  import drafts.ReadExt._
  import org.json4s.jackson.parseJson
  import org.json4s.scalaz.JsonScalaz._
  import _root_.scalaz._, Scalaz._


  val str =
    """
      |{
      |  "a": "aaaa",
      |  "x": 0
      |}""".stripMargin

  val json = parseJson(str)

  (json \ "a").read[Option[String]]
  // \/-(Some(aaaa))

  (json \ "b").read[Option[String]]
  // \/-(None)

  (json \ "a").validate[Option[String]]
  // Success(Some(aaaa))

  (json \ "b").validate[Option[String]]
  // Success(None)


  case class MyA(a: Option[String], x: Int)



  implicit val myARead = readE[MyA] { json =>
    for {
      a <- (json \ "a").read[Option[String]]
      x <- (json \ "x").read[Int]
    } yield MyA(a, x)
  }

  val myARead2 = read[MyA] { json =>
    (
      (json \ "a").validate[Option[String]] |@|
      (json \ "x").validate[Int]
    ).tupled.map(MyA.tupled)
  }

  val myARead3: JSONR[(Option[String], Int)] = read[(Option[String], Int)] {
    for {
      a <- fieldT[Option[String]](_ \ "a")
      x <- fieldT[Int](_ \ "x")
    } yield (a |@| x).tupled
  }

  implicit val myAWrite = write[MyA] { myA =>
    ("a" -> myA.a) ~
      ("x" -> myA.x)
  }


  json.read[MyA]
  // \/-(MyA(Some(aaaa),0))

  json.validate[MyA]
  // Success(MyA(Some(aaaa),0))

  MyA(Some("aaaa"), 0).toJson
  // JObject(List((a,JString(aaaa)), (x,JInt(0))))



}
