import org.json4s._
import org.json4s.jackson.prettyJson
import org.json4s.ext.scalaz.JsonScalaz._

object ReadAutoExample2 extends App {

  // beware: can lead to unexpected compilation successes

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

  // import org.json4s.ext.scalaz.JsonScalaz.auto._

  implicit def queryParamJSON = deriveJSON[QueryParam]
  implicit def cookieJSON     = deriveJSON[Cookie]
  implicit def requestJSON    = deriveJSON[Request]
  implicit def responseJSON   = deriveJSON[Response]
  implicit def entryJSON      = deriveJSON[Entry]

  val json: JValue = ??? // entry.toJson
  val res = json.read[Entry]

  println(prettyJson(json))
  println(res)


}
