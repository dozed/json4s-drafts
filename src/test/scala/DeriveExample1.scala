import org.json4s._
import org.json4s.ext.scalaz.JsonScalaz._
import org.json4s.jackson.{parseJson, prettyJson}

import scalaz._, Scalaz._
import shapeless._

object DeriveExample1 extends App {

  case class Localization(locale: String, title: String, caption: String)

  case class Photo(id: String, file: String, localizations: NonEmptyList[Localization])


  // TODO compiles, but shouldnt
  //  implicit def localizationJson: JSON[Localization] = JSON.of[Localization]
  //  implicit def photoJson: JSON[Photo] = JSON.of[Photo]

  implicit def localizationJson: JSON[Localization] = JSON.derive[Localization]
  implicit def photoJson: JSON[Photo] = JSON.derive[Photo]


  implicit def nonEmptyList[A:JSON]: JSON[NonEmptyList[A]] = JSON.of[JArray].exmap[NonEmptyList[A]](
    jarr => {
      for {
        xs  <- jarr.children.map(fromJSON[A]).sequence[Result, A]
        nel <- xs.toNel.toSuccess((UncategorizedError("empty_list_for_nel", "empty list for NonEmptyList", Nil):Error).wrapNel)
      } yield {
        nel
      }
    },
    nel => JArray(nel.list.map(x => toJSON(x)).toList)
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
