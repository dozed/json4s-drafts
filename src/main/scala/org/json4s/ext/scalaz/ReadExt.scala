package org.json4s.ext.scalaz

import org.json4s._

trait ReadExt { self: Types =>

  import scalaz._, Scalaz._

  def fieldT[A:JSONR](f: JValue => JValue): JValue => Result[A] = { json =>
    implicitly[JSONR[A]].read(f(json))
  }

  def validate2[A:JSONR](json: JValue): Result[A] = implicitly[JSONR[A]].read(json)

  implicit class StringExt(s: String) {

    def json: Option[JValue] = org.json4s.jackson.parseJsonOpt(s)

    def validate[A: JSONR]: ValidationNel[Error, A] = {
      json.toSuccessNel(InvalidFormatError("malformed json"):Error) flatMap (_.validate[A])
    }

    def read[A: JSONR]: Error \/ A = validate[A].disjunction.leftMap(_.head)

  }

}
