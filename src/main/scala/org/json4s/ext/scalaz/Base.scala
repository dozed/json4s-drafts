package org.json4s.ext.scalaz

import org.json4s._
import org.json4s.ext.scalaz.JValueExts._

import scalaz._
import std.list._
import std.option._
import syntax.std.option._
import syntax.validation._
import syntax.traverse._
import scala.collection.breakOut

trait Base { this: Types =>
  implicit def boolJSON: JSON[Boolean] = new JSON[Boolean] {
    def read(json: JValue) = json match {
      case JBool(b) => Validation.success(b)
      case x => Fail.unexpected(x, classOf[JBool])
    }

    def write(value: Boolean) = JBool(value)
  }

  implicit def intJSON: JSON[Int] = new JSON[Int] {
    def read(json: JValue) = json match {
      case JInt(x) => Validation.success(x.intValue)
      case x => Fail.unexpected(x, classOf[JInt])
    }

    def write(value: Int) = JInt(BigInt(value))
  }

  implicit def longJSON: JSON[Long] = new JSON[Long] {
    def read(json: JValue) = json match {
      case JInt(x) => Validation.success(x.longValue)
      case x => Fail.unexpected(x, classOf[JInt])
    }

    def write(value: Long) = JInt(BigInt(value))
  }

  implicit def doubleJSON: JSON[Double] = new JSON[Double] {
    def read(json: JValue) = json match {
      case JDouble(x) => Validation.success(x)
      case x => Fail.unexpected(x, classOf[JDouble])
    }

    def write(value: Double) = JDouble(value)
  }

  implicit def stringJSON: JSON[String] = new JSON[String] {
    def read(json: JValue) = json match {
      case JString(x) => Validation.success(x)
      case x => Fail.unexpected(x, classOf[JString])
    }

    def write(value: String) = JString(value)
  }

  implicit def bigintJSON: JSON[BigInt] = new JSON[BigInt] {
    def read(json: JValue) = json match {
      case JInt(x) => Validation.success(x)
      case x => Fail.unexpected(x, classOf[JInt])
    }

    def write(value: BigInt) = JInt(value)
  }

  implicit def jvalueJSON: JSON[JValue] = new JSON[JValue] {
    def read(json: JValue) = Validation.success(json)
    def write(value: JValue) = value
  }

  implicit def jobjectJSON: JSON[JObject] = new JSON[JObject] {
    def read(json: JValue) = json.asJObject.toSuccessNel[Error](UnexpectedJSONError(json, classOf[JObject]))
    def write(value: JObject) = value
  }

  implicit def listJSONR[A: JSONR]: JSONR[List[A]] = new JSONR[List[A]] {
    def read(json: JValue) = json match {
      case JArray(xs) => 
        xs.map(fromJSON[A]).sequence[({type λ[t] = ValidationNel[Error, t]})#λ, A]
      case x => Fail.unexpected(x, classOf[JArray])
    }
  }
  implicit def listJSONW[A: JSONW]: JSONW[List[A]] = new JSONW[List[A]] {
    def write(values: List[A]) = JArray(values.map(x => toJSON(x)))
  }

  implicit def optionJSONR[A: JSONR]: JSONR[Option[A]] = new JSONR[Option[A]] {
    def read(json: JValue) = json match {
      case JNothing | JNull => Validation.success(None)
      case x => fromJSON[A](x).map(some)
    }
  }
  implicit def optionJSONW[A: JSONW]: JSONW[Option[A]] = new JSONW[Option[A]] {
    def write(value: Option[A]) = value.map(x => toJSON(x)).getOrElse(JNull)
  }

  implicit def mapJSONR[A: JSONR]: JSONR[Map[String, A]] = new JSONR[Map[String, A]] {
    def read(json: JValue) = json match {
      case JObject(fs) =>
        val m = fs.map(f => fromJSON[A](f._2) map (f._1 -> _))
        val mm = m.sequence[({type λ[t] = ValidationNel[Error, t]})#λ, (String, A)]
        mm.map(_.toMap)
//        val r = m.sequence[PartialApply1Of2[ValidationNEL, Error]#Apply, (String, A)]
//        r.map(_.toMap)
//        val r = fs.map(f => fromJSON[A](f._2).map(v => (f._1, v))).sequence[PartialApply1Of2[ValidationNEL, Error]#Apply, (String, A)]
//        r.map(_.toMap)
      case x => Fail.unexpected(x, classOf[JObject])
    }
  }
  implicit def mapJSONW[A: JSONW]: JSONW[Map[String, A]] = new JSONW[Map[String, A]] {
    def write(values: Map[String, A]) = JObject(values.map { case (k, v) => JField(k, toJSON(v)) }(breakOut).toList)
  }
}
