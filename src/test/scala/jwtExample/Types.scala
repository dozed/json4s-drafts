package jwtExample

import shapeless._
import shapeless.newtype._
import shapeless.{:+:, Coproduct, _}

import scalaz._, Scalaz._

object JwtTypes extends HeaderTypes with ClaimsTypes {

  case class Jwt(headers: List[Header], claims: List[Claim])

}

trait AlgorithmTypes {


  implicit val algorithmShows = Show.show[Algorithm] {
    case Algorithm.HS256 => "HS256"
    case Algorithm.HS384 => "HS384"
    case Algorithm.HS512 => "HS512"
    case Algorithm.NONE => "none"
  }

  sealed trait Algorithm

  object Algorithm {

    case object HS256 extends Algorithm
    case object HS384 extends Algorithm
    case object HS512 extends Algorithm
    case object NONE extends Algorithm

  }

}

trait HeaderTypes extends AlgorithmTypes {

  type Header = Header.Typ :+: Header.Cty :+: Algorithm :+: CNil

  object Header {

    type Typ = Newtype[String, TypOps]
    type Cty = Newtype[String, CtyOps]

    def Typ(s: String): Typ = newtype(s)
    def Cty(s: String): Cty = newtype(s)

  }

  case class TypOps(s: String) {
    def value = s
  }

  case class CtyOps(s: String) {
    def value = s
  }

  case class AlgOps(s: String) {
    def value = s
  }

  implicit val mkTypOps = TypOps
  implicit val mkCtyOps = CtyOps
  implicit val mkAlgOps = AlgOps

}

trait ClaimsTypes {

  import Claim._


  // user-defined claims
  case class Custom(name: String, value: String)

  // StringOrUri
  type StringOrList = Coproduct.`String, List[String]`.T
  // type StringOrList = String :+: List[String] :+: CNil

  def stringOrList = Coproduct[StringOrList]

  // pre-defined claims
  type Claim = Iss :+: Sub :+: Aud :+: Exp :+: Nbf :+: Iat :+: Jti :+: Custom :+: CNil

  def claim = Coproduct[Claim]

  val a: Claim = claim(Claim.Iss("asd"))

  object Claim {

    type Iss = Newtype[String, IssOps]
    type Sub = Newtype[String, SubOps]
    type Aud = Newtype[StringOrList, AudOps]
    type Exp = Newtype[Long, ExpOps]
    type Nbf = Newtype[Long, NbfOps]
    type Iat = Newtype[Long, IatOps]
    type Jti = Newtype[String, JtiOps]

    object Iss {
      def apply(s: String): Iss = newtype(s)
      def unapply(s: Claim): Option[Iss] = s match {
        case Inl(iss) => iss.some
        case _ => none
      }
    }

    object Sub {
      def apply(s: String): Sub = newtype(s)
      def unapply(c: Claim): Option[Sub] = c match {
        case Inr(Inl(v)) => v.some
        case _ => none
      }
    }
    
    object Aud {
      def apply(s: StringOrList): Aud = newtype(s)
      def unapply(c: Claim): Option[Aud] = c match {
        case Inr(Inr(Inl(v))) => v.some
        case _ => none
      }
    }
    
    object Exp {
      def apply(s: Long): Exp = newtype(s)
      def unapply(c: Claim): Option[Exp] = c match {
        case Inr(Inr(Inr(Inl(v)))) => v.some
        case _ => none
      }
    }
    
    object Nbf {
      def apply(s: Long): Nbf = newtype(s)
      def unapply(c: Claim): Option[Nbf] = c match {
        case Inr(Inr(Inr(Inr(Inl(v))))) => v.some
        case _ => none
      }
    }
    
    object Iat {
      def apply(s: Long): Iat = newtype(s)
      def unapply(c: Claim): Option[Iat] = c match {
        case Inr(Inr(Inr(Inr(Inr(Inl(v)))))) => v.some
        case _ => none
      }
    }
    
    object Jti {
      def apply(s: String): Jti = newtype(s)
      def unapply(c: Claim): Option[Jti] = c match {
        case Inr(Inr(Inr(Inr(Inr(Inr(Inl(v))))))) => v.some
        case _ => none
      }
    }


  }

  case class IssOps(s: String) {
    def value = s
  }

  case class SubOps(s: String) {
    def value = s
  }
  case class AudOps(s: StringOrList) {
    def value = s
  }
  case class ExpOps(s: Long) {
    def value = s
  }
  case class NbfOps(s: Long) {
    def value = s
  }
  case class IatOps(s: Long) {
    def value = s
  }
  case class JtiOps(s: String) {
    def value = s
  }

  implicit val mkIssOps = IssOps
  implicit val mkSubOps = SubOps
  implicit val mkAudOps = AudOps
  implicit val mkExpOps = ExpOps
  implicit val mkNbfOps = NbfOps
  implicit val mkIatOps = IatOps
  implicit val mkJtiOps = JtiOps



}