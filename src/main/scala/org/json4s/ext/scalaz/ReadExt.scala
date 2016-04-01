package org.json4s.ext.scalaz

import org.json4s._

trait ReadExt { self: Types =>

  import scalaz._, Scalaz._

  def fieldT[A:JSONR](f: JValue => JValue): JValue => Result[A] = { json =>
    implicitly[JSONR[A]].read(f(json))
  }

  def validate2[A:JSONR](json: JValue): Result[A] = implicitly[JSONR[A]].read(json)

  implicit class StringExt(s: String) {
    def json = org.json4s.jackson.parseJson(s)
    def validate[A: JSONR]: ValidationNel[Error, A] = implicitly[JSONR[A]].read(json)
    def read[A: JSONR]: Error \/ A = implicitly[JSONR[A]].read(json).disjunction.leftMap(_.head)
  }

}
