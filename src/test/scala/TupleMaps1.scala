import org.json4s._
import org.json4s.ext.scalaz.JsonScalaz._
import org.json4s.jackson.{parseJson, prettyJson}

import scalaz.Scalaz._
import scalaz._

object TupleMaps1 extends App {

  case class Localization(locale: String, title: String, caption: String)

  case class Photo(id: String, file: String, localizations: NonEmptyList[Localization])


  implicit val localizationJson = json3("locale", "title", "caption")(Localization.unapply, Localization.apply)
  implicit val photoJson = json3("id", "file", "localizations")(Photo.unapply, Photo.apply)


  implicit def nonEmptyList[A:JSONW:JSONR]: JSON[NonEmptyList[A]] = JSON[JArray].exmap[NonEmptyList[A]](
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
