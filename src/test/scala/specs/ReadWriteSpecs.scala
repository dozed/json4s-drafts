package specs

import org.json4s._
import org.json4s.ext.scalaz.JsonScalaz._
import org.json4s.jackson.parseJson
import org.specs2.mutable.Specification

import scalaz._, Scalaz._

object ReadWriteSpecs extends Specification {

  case class Item(label: String, amount: Int, price: Double)
  case class AddressInfo(street: String, zip: String, info: DynamicJValue)

  "Parse item in Monadic style" in {

    val json: JValue =
      parseJson("""
        |[
        |  {
        |    "label": "foo item",
        |    "amount": { "value": 200 },
        |    "price": { "value": 1.99 }
        |  },
        |  {
        |    "label": "bar item",
        |    "amount": { "value": 100 },
        |    "price": { "value": 2.50 }
        |  }
        |]
      """.stripMargin)

    // TODO support syntax for symmetric case
    implicit val itemJSONR: JSONR[Item] = JSONR.instanceE[Item] { json =>
      for {
        label <- (json \ "label").read[String]
        amount <- (json \ "amount" \ "value").read[Int]
        price <- (json \ "price" \ "value").read[Double]
      } yield Item(label, amount, price)
    }

    implicit val itemJSONW: JSONW[Item] = JSONW.instance[Item] { item =>
      makeObj(
        ("label" -> toJSON(item.label)) ::
        ("amount" -> makeObj(("value" -> toJSON(item.amount)) :: Nil)) ::
        ("price" -> makeObj(("value" -> toJSON(item.price)) :: Nil)) :: Nil
      )
    }

    json.validate[List[Item]] must beLike[Result[List[Item]]] {
      case Success(xs) =>
        xs must haveSize(2)

        (xs.toJson === json) must beTrue
    }

  }

  "Parse AddressInfo in Applicative style" in {

    val text =
      """
        |{
        |  "street" : "Manhattan 2",
        |  "zip" : "00223",
        |  "info" : { "verified": true }
        |}
      """.stripMargin

    val json: JValue = parseJson(text)


    implicit val dynamicJValueJson = JSON.instance[DynamicJValue](
      json => DynamicJValue.dyn(json).successNel,
      _.raw
    )

    // TODO support syntax for symmetric case
    implicit val adressInfoJSONR: JSONR[AddressInfo] = AddressInfo.applyJSON(
      field[String]("street"),
      field[String]("zip"),
      field[DynamicJValue]("info")
    )

    // TODO simplify
    implicit val addressInfoJSONW: JSONW[AddressInfo] = JSONW.instance[AddressInfo] { info =>
      makeObj(
        ("street" -> toJSON(info.street)) ::
        ("zip" -> toJSON(info.zip)) ::
        ("info" -> toJSON(info.info)) :: Nil
      )
    }

    json.validate[AddressInfo] must beLike[Result[AddressInfo]] {
      case Success(info) =>

        info.street must_== "Manhattan 2"
        info.zip must_== "00223"
        info.info.raw must_== JObject("verified" -> JBool(true))
    }


  }


}
