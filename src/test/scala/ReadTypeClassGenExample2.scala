import org.json4s._
import org.json4s.jackson.prettyJson
import org.json4s.ext.scalaz.JsonScalaz._

object ReadTypeClassGenExample2 extends App {


  case class QueryParam(name: String, value: String)
  // TODO expires as value of type Any crashes the compiler
  case class Cookie(name: String, value: String, expires: Long, httpOnly: Boolean, secure: Boolean)
  case class Request(method: String, url: String, queryString: List[QueryParam], cookies: List[Cookie])
  case class Response(status: Int)
  case class Entry(request: Request, response: Response)


  val entry = Entry(
    Request(
      "GET",
      "/",
      List(
        QueryParam("name", "value"),
        QueryParam("name", "value")
      ),
      List(
        Cookie("name", "value", 0, true, true)
      )
    ),
    Response(200)
  )

  val json: JValue = entry.toJson
  val res = json.read[Entry]

  println(prettyJson(json))
  println(res)


}
