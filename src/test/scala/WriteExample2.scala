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

  implicit val algorithmWrite = JSONW[String].contramap[Algorithm] {
    case Algorithm.HS256 => "HS256"
    case Algorithm.HS384 => "HS384"
    case Algorithm.HS512 => "HS512"
    case Algorithm.NONE => "NONE"
  }

  val json = (Algorithm.HS512:Algorithm).toJson
  println(json)
  // JString(HS512)



  implicit val algorithmRead: JSONR[Algorithm] = JSONR[String].map(_.toUpperCase).emap {
    case "HS256" => Algorithm.HS256.successNel
    case "HS384" => Algorithm.HS384.successNel
    case "HS512" => Algorithm.HS512.successNel
    case "NONE" => Algorithm.NONE.successNel
    case x => Fail.apply("", "one of: HS256, HS384, HS512, NONE", List(x))
  }

  println(json.read[Algorithm].require)


}
