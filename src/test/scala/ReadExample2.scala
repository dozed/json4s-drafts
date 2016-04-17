import org.json4s._
import org.json4s.ext.scalaz.JsonScalaz._
import org.json4s.jackson.parseJson

import scalaz._, Scalaz._

object ReadExample2 extends App {



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



  implicit val myARead = JSONR.instanceE[MyA] { json =>
    for {
      a <- (json \ "a").read[Option[String]]
      x <- (json \ "x").read[Int]
    } yield MyA(a, x)
  }

  val myARead2 = JSONR.instance[MyA] { json =>
    (
      (json \ "a").validate[Option[String]] |@|
      (json \ "x").validate[Int]
    ).tupled.map(MyA.tupled)
  }

  val myARead3 = JSONR.instance[(Option[String], Int)] {
    for {
      a <- fieldT[Option[String]](_ \ "a")
      x <- fieldT[Int](_ \ "x")
    } yield (a |@| x).tupled
  }

  implicit val myAWrite = JSONW.instance[MyA] { myA =>
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
