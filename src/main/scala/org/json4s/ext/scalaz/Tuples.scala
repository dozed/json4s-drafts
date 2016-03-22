package org.json4s.ext.scalaz

import org.json4s._
import _root_.scalaz._
import syntax.validation._

trait Tuples { this: Types =>
  implicit def Tuple2JSON[A: JSON, B: JSON]: JSON[(A, B)] = new JSON[(A, B)] {
    def read(json: JValue) = json match {
      case JArray(a :: b :: _) =>
        Apply[Result].apply2(fromJSON[A](a),fromJSON[B](b))(Tuple2.apply)
      case x => Fail.unexpected(x, classOf[JArray])
    }

    def write(value: (A, B)) = JArray(toJSON(value._1) :: toJSON(value._2) :: Nil)
  }

  implicit def Tuple3JSON[A: JSON, B: JSON, C: JSON]: JSON[(A, B, C)] = new JSON[(A, B, C)] {
    def read(json: JValue) = json match {
      case JArray(a :: b :: c :: _) =>
        Apply[Result].apply3(fromJSON[A](a),fromJSON[B](b),fromJSON[C](c))(Tuple3.apply)
      case x => Fail.unexpected(x, classOf[JArray])
    }

    def write(value: (A, B, C)) = JArray(toJSON(value._1) :: toJSON(value._2) :: toJSON(value._3) :: Nil)
  }

  implicit def Tuple4JSON[A: JSON, B: JSON, C: JSON, D: JSON]: JSON[(A, B, C, D)] = new JSON[(A, B, C, D)] {
    def read(json: JValue) = json match {
      case JArray(a :: b :: c :: d :: _) =>
        Apply[Result].apply4(fromJSON[A](a),fromJSON[B](b),fromJSON[C](c),fromJSON[D](d))(Tuple4.apply)
      case x => Fail.unexpected(x, classOf[JArray])
    }

    def write(value: (A, B, C, D)) = JArray(toJSON(value._1) :: toJSON(value._2) :: toJSON(value._3) :: toJSON(value._4) :: Nil)
  }

  implicit def Tuple5JSON[A: JSON, B: JSON, C: JSON, D: JSON, E: JSON]: JSON[(A, B, C, D, E)] = new JSON[(A, B, C, D, E)] {
    def read(json: JValue) = json match {
      case JArray(a :: b :: c :: d :: e :: _) =>
        Apply[Result].apply5(fromJSON[A](a),fromJSON[B](b),fromJSON[C](c),fromJSON[D](d),fromJSON[E](e))(Tuple5.apply)
      case x => Fail.unexpected(x, classOf[JArray])
    }

    def write(value: (A, B, C, D, E)) = JArray(toJSON(value._1) :: toJSON(value._2) :: toJSON(value._3) :: toJSON(value._4) :: toJSON(value._5) :: Nil)
  }

  implicit def Tuple6JSON[A: JSON, B: JSON, C: JSON, D: JSON, E: JSON, F: JSON]: JSON[(A, B, C, D, E, F)] = new JSON[(A, B, C, D, E, F)] {
    def read(json: JValue) = json match {
      case JArray(a :: b :: c :: d :: e :: f :: _) =>
        Apply[Result].apply6(fromJSON[A](a),fromJSON[B](b),fromJSON[C](c),fromJSON[D](d),fromJSON[E](e),fromJSON[F](f))(Tuple6.apply)
      case x => Fail.unexpected(x, classOf[JArray])
    }

    def write(value: (A, B, C, D, E, F)) = JArray(toJSON(value._1) :: toJSON(value._2) :: toJSON(value._3) :: toJSON(value._4) :: toJSON(value._5) :: toJSON(value._6) :: Nil)
  }
}
