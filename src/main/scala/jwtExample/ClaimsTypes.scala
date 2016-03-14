package jwtExample

import shapeless._
import shapeless.newtype._
import shapeless.{:+:, Coproduct, _}

object ClaimsTypes {

  import Claim._


  // user-defined claims
  case class Custom(name: String, value: String)

  type StringOrList = Coproduct.`String, List[String]`.T
  // type StringOrList = String :+: List[String] :+: CNil

  def stringOrList = Coproduct[StringOrList]

  // pre-defined claims
  type Claim = Iss :+: Sub :+: Aud :+: Exp :+: Nbf :+: Iat :+: Jti :+: Custom :+: CNil

  def claim = Coproduct[Claim]

  val a: Claim = claim(Claim.Iss("asd"))

  object Claim {

    type Iss = Newtype[String, IssOps]
    type Sub = Newtype[StringOrList, SubOps]
    type Aud = Newtype[String, AudOps]
    type Exp = Newtype[Long, ExpOps]
    type Nbf = Newtype[Long, NbfOps]
    type Iat = Newtype[Long, IatOps]
    type Jti = Newtype[String, JtiOps]

    def Iss(s: String): Iss = newtype(s)
    def Sub(s: StringOrList): Sub = newtype(s)
    def Aud(s: String): Aud = newtype(s)
    def Exp(s: Long): Exp = newtype(s)
    def Nbf(s: Long): Nbf = newtype(s)
    def Iat(s: Long): Iat = newtype(s)
    def Jti(s: String): Jti = newtype(s)

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