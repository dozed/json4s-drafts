package org.json4s.ext.scalaz

import org.json4s._

trait ReadExt { self: Types =>

  import scalaz._, Scalaz._

  // validation
  def read[A](f: JValue => Result[A]) = new JSONR[A] {
    def read(json: JValue) = f(json)
  }

  // either
  def readE[A](f: JValue => Error \/ A) = new JSONR[A] {
    def read(json: JValue) = f(json).validationNel
  }

  // lookup
  def readL[A:JSONR] = implicitly[JSONR[A]]


  def fieldT[A:JSONR](f: JValue => JValue): JValue => Result[A] = { json =>
    implicitly[JSONR[A]].read(f(json))
  }

  def validate2[A:JSONR](json: JValue): Result[A] = implicitly[JSONR[A]].read(json)

  implicit class JArrayOps(j: JArray) {
    def head: JValue = j.children.head
    def tail: JValue = JArray(j.children.tail)
  }

  implicit class StringExt(s: String) {
    def json = org.json4s.jackson.parseJson(s)
    def validate[A: JSONR]: ValidationNel[Error, A] = implicitly[JSONR[A]].read(json)
    def read[A: JSONR]: Error \/ A = implicitly[JSONR[A]].read(json).disjunction.leftMap(_.head)
  }

  implicit class JSONRExt[A](fa: JSONR[A]) {
    def orElse[B >: A](fa2: JSONR[B]): JSONR[B] = new JSONR[B] {
      override def read(json: JValue): Result[B] = {
        fa.read(json) orElse fa2.read(json)
      }
    }
    def |[B >: A](fa2: JSONR[B]): JSONR[B] = orElse(fa2)
  }

}
