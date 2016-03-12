import drafts.ReadExt._
import drafts.ReadExt.JSONRExt
import drafts.ReadExt.JSONRExt._
import org.json4s._
import org.json4s.jackson.parseJson
import org.json4s.scalaz.JsonScalaz._

import _root_.scalaz._, Scalaz._

object ParseExample3 extends App {

  val json =
      """
        |{
        |  "i": 100,
        |  "s": "hey ho"
        |}
      """.stripMargin

  case class Foo(i: Int, s: String)

  implicit val aInstance = JSONRExt[Foo]

  val foo = Foo(0, "foo")

  val res: Result[Foo] = parseJson(json).validate[Foo]
  // Success(Foo(100,hey ho))
  println(res)

  val res2: Result[Foo] = parseJson("{}").validate[Foo]
  // Failure(NonEmptyList(UnexpectedJSONError(JNothing,class org.json4s.JsonAST$JInt), UnexpectedJSONError(JNothing,class org.json4s.JsonAST$JString)))
  println(res2)

}


