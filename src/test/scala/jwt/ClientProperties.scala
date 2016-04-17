package jwt

import org.json4s.ext.scalaz.JsonScalaz._

import scalaz.Scalaz._
import scalaz._

/**
  * ClientProperty represents properties of a client. It can be used as a custom claim in a JWT.
  */
object ClientProperties {

  case class ClientProperty(
    isUser: Option[String] = none[String],
    isViewer: Set[String] = Set.empty[String],
    isOwner: Set[String] = Set.empty[String]
  )

  val KEY_CLIENT_PROPERTY = "cp"


  // constructors
  def isUser(userId: String) = ClientProperty(isUser = userId.some)
  def isViewer(documentId: String) = ClientProperty(isViewer = Set(documentId))
  def isOwner(documentId: String) = ClientProperty(isOwner = Set(documentId))

  implicit lazy val clientPropShow = Show.showFromToString[ClientProperty]

  implicit lazy val clientPropJSON = JSON.derive[ClientProperty]

  implicit lazy val clientPropEqual = Equal.equal[ClientProperty] {
    (a, b) =>
      a.isUser === b.isUser &&
      a.isOwner === b.isOwner &&
      a.isViewer === b.isViewer
  }

  // add two ClientProperty values
  implicit lazy val clientPropMonoid = Monoid.instance[ClientProperty](
    (a, b) => {
      val user: Option[String] = a.isUser orElse b.isUser
      val isViewer: Set[String] = a.isViewer union b.isViewer
      val isOwner: Set[String] = a.isOwner union b.isOwner

      ClientProperty(user, isViewer, isOwner)
    },
    ClientProperty()
  )

  // helpers
  def grantUser(user: String, props: ClientProperty) = isUser(user) mappend props
  def grantViewer(document: String, props: ClientProperty) = isViewer(document) mappend props
  def grantOwner(document: String, props: ClientProperty) = isOwner(document) mappend props

  // create a JWT with a ClientProperty payload
  def createClientPropertyToken(clientProps: ClientProperty, secret: String, algorithm: Algorithm) = {
    val claims = List[Claim](Claim.Custom(KEY_CLIENT_PROPERTY, clientProps.toJson.nospaces))
    val token = Jwt.sign(claims, secret, Algorithm.HS512)
    token
  }

}

