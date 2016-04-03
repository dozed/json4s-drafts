import org.json4s._
import org.json4s.ext.scalaz.JsonScalaz._
import org.json4s.jackson.{parseJson, prettyJson}

import scalaz._, Scalaz._
import shapeless._

object deriveExample1 extends App {

  case class Localization(locale: String, title: String, caption: String)

  case class Photo(id: String, file: String, localizations: NonEmptyList[Localization])


  def derive[A](implicit gen: LabelledGeneric[A]) = new {
    def json[R](implicit writeRepr: JSONW[gen.Repr], readRepr: JSONR[gen.Repr]) = new JSON[A] {
      override def read(json: JValue): Result[A] = readRepr.read(json) map gen.from
      override def write(value: A): JValue = writeRepr.write(gen.to(value))
    }

    def jsonw[R](implicit writeRepr: JSONW[gen.Repr]) = new JSONW[A] {
      override def write(value: A): JValue = writeRepr.write(gen.to(value))
    }

    def jsonr[R](implicit readRepr: JSONR[gen.Repr]) = new JSONR[A] {
      override def read(json: JValue): Result[A] = readRepr.read(json) map gen.from
    }
  }


  implicit def x1: JSON[Localization] = derive[Localization].json
  implicit def x2: JSON[Photo] = derive[Photo].json

  implicit def nonEmptyList[A:JSONW:JSONR]: JSON[NonEmptyList[A]] = JSON.of[JArray].exmap[NonEmptyList[A]](
    jarr => {
      for {
        xs  <- jarr.children.map(fromJSON[A]).sequence[Result, A]
        nel <- xs.toNel.toSuccess((UncategorizedError("empty_list_for_nel", "empty list for NonEmptyList", Nil):Error).wrapNel)
      } yield {
        nel
      }
    },
    nel => JArray(nel.list.map(x => toJSON(x)))
  )

  // TODO
  // - derive a JSON[A] from JSONR[A] and JSONW[A]
  //  implicit def jsonFromJSONRW[A](implicit readA: JSONR[A], writeA: JSONW[A]): JSON[A] = new JSON[A] {
  //    override def read(json: JValue): Result[A] = readA.read(json)
  //    override def write(value: A): JValue = writeA.write(value)
  //  }

  val photo1 = Photo("id", "file", Localization("locale", "title", "caption").wrapNel)

  val json = parseJson(
    """
      |{
      |  "id": "id",
      |  "file": "file",
      |  "localizations": [{
      |    "locale": "locale",
      |    "title": "title",
      |    "caption": "caption"
      |  }]
      |}
    """.stripMargin)

  println(prettyJson(photo1.toJson))

  println(json.validate[Photo])


}
