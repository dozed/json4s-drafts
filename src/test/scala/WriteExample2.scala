import org.json4s.ext.scalaz.JsonScalaz._
import scalaz._, Scalaz._

object WriteExample2 extends App {

  sealed trait Algorithm

  object Algorithm {
    case object HS256 extends Algorithm
    case object HS384 extends Algorithm
    case object HS512 extends Algorithm
    case object NONE extends Algorithm
  }

  implicit val algorithmWrite = JSON.writeL[String].contramap[Algorithm] {
    case Algorithm.HS256 => "HS256"
    case Algorithm.HS384 => "HS384"
    case Algorithm.HS512 => "HS512"
    case Algorithm.NONE => "NONE"
  }

  val json = (Algorithm.HS512:Algorithm).toJson
  println(json)
  // JString(HS512)


}
