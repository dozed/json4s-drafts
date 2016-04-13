package jwt

import ClientProperties._

import org.scalacheck.Prop.forAll
import org.specs2.mutable.Specification
import org.specs2.ScalaCheck
import org.specs2.scalaz.ScalazMatchers

import scalaz._
import Scalaz._

class ClientPropertySpecs extends Specification with ScalazMatchers with ScalaCheck {

  val userClaim = ClientProperty(isUser = "42".some)
  val viewerClaim = ClientProperty(isViewer = Set("100"))
  val ownerClaim = ClientProperty(isOwner = Set("200"))

  "Claim constructors construct claims" in {

    isUser("42") should equal(userClaim)
    isViewer("100") should equal(viewerClaim)
    isOwner("200") should equal(ownerClaim)

  }

  "Combining two user claims, takes the user id from the first claim" in {

    isUser("23") mappend isUser("42") should equal(ClientProperty(isUser = "23".some))

    forAll { (id1: String, id2: String) =>
      isUser(id1) mappend isUser(id2) should equal(isUser(id1))
    }

  }

  "There is the empty ClientProperty (zero)" in {

    ClientProperty() should equal(ClientProperty(None, Set.empty, Set.empty))

    Monoid[ClientProperty].zero should equal(ClientProperty())

  }

  "Combining two claims, combines also the viewer claims and owner claims (mappend)" in {

    ClientProperty(isViewer = Set("100", "102"), isOwner = Set("100", "101")) |+| ClientProperty(isViewer = Set("100", "101"), isOwner = Set("100")) should equal(ClientProperty(isViewer = Set("100", "101", "102"), isOwner = Set("100", "101")))

    forAll { (owners1: Set[String], owners2: Set[String], viewers1: Set[String], viewers2: Set[String]) =>

      val claim1 = ClientProperty(isViewer = viewers1, isOwner = owners1)
      val claim2 = ClientProperty(isViewer = viewers2, isOwner = owners2)

      val claim3 = ClientProperty(isViewer = viewers1 union viewers2, isOwner = owners1 union owners2)

      claim1 |+| claim2 should equal(claim3)

    }

  }

  "Granting a user claim overrides an existing user claim" in {

    grantUser("42", ClientProperty()) should equal(isUser("42"))
    grantUser("23", ClientProperty(isUser = "42".some)) should equal(isUser("42"))

  }



}
