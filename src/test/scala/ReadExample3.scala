import drafts.ReadExt._
import drafts.ReadExt.JSONRExt
import drafts.ReadExt.JSONRExt._
import org.json4s._
import org.json4s.jackson.parseJson
import org.json4s.scalaz.JsonScalaz._

import _root_.scalaz._, Scalaz._

object ReadExample3 extends App {

  val json =
      """
        |{
        |  "i": 100,
        |  "s": "hey ho",
        |  "x": ["foo", "bar"],
        |  "a": {
        |    "x": 200
        |  }
        |}
      """.stripMargin

  case class A(x: Int)
  case class Foo(i: Int, s: String, x: List[String], a: A)

  val foo = Foo(0, "foo", List("foo", "bar"), A(200))

  val res: Result[Foo] = parseJson(json).validate[Foo]
  // Success(Foo(100,hey ho,List(foo, bar),A(200)))
  println(res)

  val json2 =
    parseJson("""
      |{
      |  "i": 100,
      |  "s": "hey ho",
      |  "x": "foo",
      |  "a": 200
      |}
    """.stripMargin)


  val res2: Result[Foo] = json2.validate[Foo]
  // Failure(NonEmptyList(UnexpectedJSONError(JString(foo),class org.json4s.JsonAST$JArray), UncategorizedError(x,Could not read value,List())))
  println(res2)

}


