package org.json4s.ext.scalaz

import org.json4s._

trait ReadExt { self: Types =>

  import shapeless.newtype._
  import shapeless.ops.coproduct.Inject
  import shapeless.{Coproduct, _}

  import _root_.scalaz._
  import Scalaz._

  def read[A](f: JValue => Result[A]) = new JSONR[A] {
    def read(json: JValue) = f(json)
  }

  // either
  def readE[A](f: JValue => Error \/ A) = new JSONR[A] {
    def read(json: JValue) = f(json).validationNel
  }

  def readR[A](r: JSONR[A]): JSONR[A] = r

  def fieldT[A:JSONR](f: JValue => JValue): JValue => Result[A] = { json =>
    implicitly[JSONR[A]].read(f(json))
  }

  def validate2[A:JSONR](json: JValue): Result[A] = implicitly[JSONR[A]].read(json)

  // lookup
  def readL[A:JSONR] = implicitly[JSONR[A]]

  implicit class JArrayOps(j: JArray) {
    def head: JValue = j.children.head
    def tail: JValue = JArray(j.children.tail)
  }

  implicit class StringExt(s: String) {
    def json = org.json4s.jackson.parseJson(s)
    def validate[A: JSONR]: ValidationNel[Error, A] = implicitly[JSONR[A]].read(json)
    def read[A: JSONR]: Error \/ A = implicitly[JSONR[A]].read(json).disjunction.leftMap(_.head)

    def validateC[T, C <: Coproduct](implicit read: JSONR[T], inj: Inject[C, T]): Result[C] = {
      read.read(json).map(t => Coproduct[C](t))
    }
  }

  implicit class JSONRExt[A](fa: JSONR[A]) {
    def orElse[B >: A](fa2: JSONR[B]): JSONR[B] = new JSONR[B] {
      override def read(json: JValue): Result[B] = {
        fa.read(json) orElse fa2.read(json)
      }
    }
    def |[B >: A](fa2: JSONR[B]): JSONR[B] = orElse(fa2)
  }

  // generate JSONR for a Newtype
  implicit def readNewtype[A, Ops](implicit read: JSONR[A]): JSONR[Newtype[A, Ops]] = {
    read.map((x: A) => newtype[A, Ops](x))
  }


}
