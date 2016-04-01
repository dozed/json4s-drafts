package org.json4s.ext.scalaz

import org.json4s._

import scalaz._
import std.option._
import syntax.applicative._
import syntax.validation._
import syntax.contravariant._
import Validation._
import shapeless.ops.coproduct.Inject
import shapeless.Coproduct

trait Types extends Base {
  type Result[+A] = ValidationNel[Error, A]

  sealed abstract class Error extends Product with Serializable
  case class UnexpectedJSONError(was: JValue, expected: Class[_ <: JValue]) extends Error
  case class NoSuchFieldError(name: String, json: JValue) extends Error
  case class UncategorizedError(key: String, desc: String, args: List[Any]) extends Error

  object Fail {
    def apply[A](key: String, desc: String, args: List[Any]): Result[A] =
      (UncategorizedError(key, desc, args):Error).failureNel

    def apply[A](key: String, desc: String): Result[A] =
      (UncategorizedError(key, desc, Nil):Error).failureNel

    def unexpected[A](was: JValue, expected: Class[_ <: JValue]): Result[A] =
      (UnexpectedJSONError(was, expected):Error).failureNel

    def noSuchField[A](name: String, json: JValue): Result[A] =
      (NoSuchFieldError(name, json):Error).failureNel
  }

  implicit class ValidationExt[A](res: Result[A]) {
    def require: A = res.fold(_ => sys.error("require"), identity)
  }

  implicit class EitherExt[A](res: Error \/ A) {
    def require: A = res.fold(_ => sys.error("require"), identity)
  }

  implicit def JValueMonoid: Monoid[JValue] = Monoid.instance(_ ++ _, JNothing)
  implicit def JValueEqual: Equal[JValue] = Equal.equalA

  type JValueTransform = JValue => Result[JValue]

  trait JSONR[A] { self =>
    def read(json: JValue): Result[A]
  }

  trait JSONW[A] {
    def write(value: A): JValue
  }

  trait JSON[A] extends JSONR[A] with JSONW[A]

  implicit val jsonrMonad = new Monad[JSONR] {
    override def map[A, B](fa: JSONR[A])(f: (A) => B): JSONR[B] = new JSONR[B] {
      override def read(json: JValue): Result[B] = {
        fa.read(json) map f
      }
    }

    override def point[A](a: => A): JSONR[A] = new JSONR[A] {
      override def read(json: JValue): Result[A] = a.successNel
    }

    override def bind[A, B](fa: JSONR[A])(f: (A) => JSONR[B]): JSONR[B] = new JSONR[B] {
      override def read(json: JValue): Result[B] = {
        fa.read(json) match {
          case Success(a) => f(a).read(json)
          case Failure(error) => Failure(error)
        }
      }
    }
  }

  implicit val jsonwContravariant = new Contravariant[JSONW] {
    override def contramap[A, B](r: JSONW[A])(f: (B) => A): JSONW[B] = new JSONW[B] {
      override def write(value: B): JValue = {
        r.write(f(value))
      }
    }
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

  implicit class JSONExt[A](fa: JSON[A]) {

    def xmap[B](f1: A => B, f2: B => A): JSON[B] = new JSON[B] {
      override def write(value: B): JValue = (fa:JSONW[A]).contramap(f2).write(value)

      override def read(json: JValue): Result[B] = (fa:JSONR[A]).map(f1).read(json)
    }

    def exmap[B](f1: A => Result[B], f2: B => A): JSON[B] = new JSON[B] {
      override def write(value: B): JValue = (fa:JSONW[A]).contramap(f2).write(value)

      override def read(json: JValue): Result[B] = (fa:JSONR[A]).emap(f1).read(json)
    }

  }

  case class JSONWContext[C, A](a: (C, A))

  object JSON {

    def transform(f: JValueTransform): JValueTransform = f

    def json[A:JSONR:JSONW]: JSON[A] = new JSON[A] {
      override def read(json: JValue): Result[A] = implicitly[JSONR[A]].read(json)
      override def write(value: A): JValue = implicitly[JSONW[A]].write(value)
    }

    // validation
    def read[A](f: JValue => Result[A]): JSONR[A] = new JSONR[A] {
      def read(json: JValue) = f(json)
    }

    // either
    def readE[A](f: JValue => Error \/ A): JSONR[A] = new JSONR[A] {
      def read(json: JValue) = f(json).validationNel
    }

    // lookup
    def readL[A:JSONR]: JSONR[A] = implicitly[JSONR[A]]

    def writeL[A:JSONW]: JSONW[A] = implicitly[JSONW[A]]

    object write {

      def apply[A](f: A => JValue): JSONW[A] = new JSONW[A] {
        override def write(a: A): JValue = f(a)
      }

      def nil[A]: JSONW[A] = write[A](_ => JNothing)

      def context[C, A](f: (C, A) => JValue): JSONW[JSONWContext[C, A]] = write[JSONWContext[C, A]](ca => f.tupled(ca.a))

    }


  }

  implicit def Result2JSONR[A](f: JValue => Result[A]): JSONR[A] = new JSONR[A] {
    def read(json: JValue) = f(json)
  }

  implicit class JSONROps(json: JValue) {
    def validate[A: JSONR]: ValidationNel[Error, A] = implicitly[JSONR[A]].read(json)
    def read[A: JSONR]: Error \/ A = implicitly[JSONR[A]].read(json).disjunction.leftMap(_.head)

    def validateC[A:JSONR, C <: Coproduct](implicit inj: Inject[C, A]): Result[C] = {
      implicitly[JSONR[A]].read(json).map(t => Coproduct[C](t))
    }
  }

  def fromJSON[A: JSONR](json: JValue): Result[A] = implicitly[JSONR[A]].read(json)
  def toJSON[A: JSONW](value: A): JValue = implicitly[JSONW[A]].write(value)

  def field[A: JSONR](name: String)(json: JValue): Result[A] = json match {
    case JObject(fs) => 
      fs.find(_._1 == name)
        .map(f => implicitly[JSONR[A]].read(f._2))
        .orElse(implicitly[JSONR[A]].read(JNothing).fold(_ => none, x => some(Success(x))))
        .getOrElse(Fail.noSuchField(name, json))
    case x => Fail.unexpected(x, classOf[JObject])
  }

  type EitherNel[+a] = NonEmptyList[Error] \/ a
  def validate[A: JSONR](name: String) = Kleisli(field[A](name)).mapK[EitherNel, A](_.disjunction)
  implicit def function2EitherNel[A](f: A => Result[A]): (A => EitherNel[A]) = (a: A) => f(a).disjunction
  implicit def kleisli2Result[A](v: Kleisli[EitherNel, JValue, A]): JValue => Result[A] = v.run.andThen(_.validation)

  def makeObj(fields: Traversable[(String, JValue)]): JObject = 
    JObject(fields.toList.map { case (n, v) => JField(n, v) })
}

object JsonScalaz extends Types with JValueExts with Lifting with Base with Dsl with ReadExt with WriteExt with Tuples with JsonShapeless
