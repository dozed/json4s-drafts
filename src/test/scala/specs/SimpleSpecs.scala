package specs

import org.json4s._
import org.json4s.ext.scalaz.JsonScalaz._
import org.specs2.mutable.Specification

import scalaz._, Scalaz._

object SimpleSpecs extends Specification {

  case class WithDefaults(
    i: Int,
    s: String = "b"
  )

  implicit val withDefaultEquals = Equal.equalA[WithDefaults]

  val withDefaultsJSON = JSON.derive[WithDefaults]

  "Simple specs" in {

    val value0 = WithDefaults(42, "a")
    val expectedJson0 = JObject(
      "i" -> JInt(42),
      "s" -> JString("a")
    )


    val value1 = WithDefaults(42)
    val expectedJson1 = JObject(
      "i" -> JInt(42),
      "s" -> JString("b")
    )

    val json0 = withDefaultsJSON.write(value0)
    val value01 = withDefaultsJSON.read(expectedJson0)

    json0 should beEqualTo(expectedJson0)

    value0 should beEqualTo(value01.require)

    val json1 = withDefaultsJSON.write(value1)
    val value11 = withDefaultsJSON.read(expectedJson1)

    json1 should beEqualTo(expectedJson1)
    value1 should beEqualTo(value11.require)


  }


}
