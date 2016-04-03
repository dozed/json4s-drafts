package specs

import org.json4s._
import org.json4s.ext.scalaz.JsonScalaz._
import org.specs2.mutable.Specification

object ResolutionPrioritySpecs extends Specification {

  case class CC(i: Int, s: String)

  case class CC2(i: Int, s: String)

  object CC2 {

    implicit val ccJSON: JSON[CC2] = JSON.of[(Int, String)].xmap[CC2](
      tp => CC2(tp._1, tp._2),
      cc => (cc.i, cc.s)
    )

  }

  val value0 = CC(42, "a")

  val expectedJson0 = JObject(
    "i" -> JInt(42),
    "s" -> JString("a")
  )

  val value1 = CC2(42, "a")

  val expectedJson1 = JArray(List(
    JInt(42),
    JString("a")
  ))


  "Without a typeclass instance compilation fails" in {

    shapeless.test.illTyped("""toJSON(value0)""")
    shapeless.test.illTyped("""fromJSON[CC](expectedJson0)""")

    success

  }

  "An instance can be derived explicitely" in {

    implicit val ccJSON = deriveJSON[CC]

    val json0 = toJSON(value0)
    val readValue0 = fromJSON[CC](expectedJson0)

    json0 should beEqualTo(expectedJson0)
    readValue0.require should beEqualTo(value0)

  }

  "A custom conversion can be defined" in {

    val json1 = toJSON(value1)
    val readValue1 = fromJSON[CC2](expectedJson1)

    json1 should beEqualTo(expectedJson1)
    readValue1.require should beEqualTo(value1)

  }

  "An instance can be derived implicitly" in {

    import org.json4s.ext.scalaz.JsonScalaz.auto._

    val json0 = toJSON(value0)
    val readValue0 = fromJSON[CC](expectedJson0)

    json0 should beEqualTo(expectedJson0)
    readValue0.require should beEqualTo(value0)

  }

  "An instance in implicit scope overrides implicit derivation" in {

    import org.json4s.ext.scalaz.JsonScalaz.auto._

    implicit val ccJSON: JSON[CC2] = JSON.of[(Int, String)].xmap[CC2](
      tp => CC2(tp._1, tp._2),
      cc => (cc.i, cc.s)
    )

    val json1 = toJSON(value1)
    val readValue1 = fromJSON[CC2](expectedJson1)

    json1 should beEqualTo(expectedJson1)
    readValue1.require should beEqualTo(value1)

  }


}
