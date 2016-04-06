import scalaz._, Scalaz._
import org.json4s._

object TypeAliasExample1 extends App {

  import org.json4s.ext.scalaz.JsonScalaz._

  type Url = String

  val url: Url = "http://www.example.org"

  JNothing.read[Url]
  url.toJson

}
