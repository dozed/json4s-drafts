package org.json4s.ext.scalaz

import org.json4s._

trait ReadExt { self: Types =>

  import scalaz._, Scalaz._

  def fieldT[A:JSONR](f: JValue => JValue): JValue => Result[A] = { json =>
    implicitly[JSONR[A]].read(f(json))
  }

  def validate2[A:JSONR](json: JValue): Result[A] = implicitly[JSONR[A]].read(json)

  implicit class JArrayExt(v: JArray) {
    def head: JValue = v.children.head
    def tail: JValue = JArray(v.children.tail)
  }

  implicit class JValueExt(v: JValue) {
    def isDefined: Boolean = !isEmpty

    def isEmpty = v match {
      case JNothing => true
      case JNull => true
      case _ => false
    }
  }

  implicit class StringExt(s: String) {
    def json = org.json4s.jackson.parseJson(s)
    def validate[A: JSONR]: ValidationNel[Error, A] = implicitly[JSONR[A]].read(json)
    def read[A: JSONR]: Error \/ A = implicitly[JSONR[A]].read(json).disjunction.leftMap(_.head)
  }

  implicit class JSONRExt[A](fa: JSONR[A]) {

    def emap[B](f: A => Result[B]): JSONR[B] = new JSONR[B] {
      override def read(json: JValue): Result[B] = {
        fa.read(json) match {
          case Success(a) => f(a)
          case f@Failure(error) => f
        }
      }
    }

    def orElse[B >: A](fa2: JSONR[B]): JSONR[B] = new JSONR[B] {
      override def read(json: JValue): Result[B] = {
        fa.read(json) orElse fa2.read(json)
      }
    }

    def |[B >: A](fa2: JSONR[B]): JSONR[B] = orElse(fa2)

    def readE1(json: JValue): Error \/ A = fa.read(json).disjunction.leftMap(_.head)

  }

}
