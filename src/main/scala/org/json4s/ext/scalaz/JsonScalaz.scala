package org.json4s.ext.scalaz

import org.json4s._

import scalaz._
import std.option._
import syntax.applicative._
import syntax.validation._
import shapeless._
import shapeless.ops.coproduct.Inject
import shapeless.Coproduct
import shapeless.:+:

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

  implicit def JValueMonoid: Monoid[JValue] = Monoid.instance(_ ++ _, JNothing)
  implicit def JValueEqual: Equal[JValue] = Equal.equalA

  trait JSONR[A] {
    def read(json: JValue): Result[A]
  }

  trait JSONW[A] {
    def write(value: A): JValue
  }

  trait JSON[A] extends JSONR[A] with JSONW[A]

  implicit val jsonrFunctor = new Functor[JSONR] {
    override def map[A, B](fa: JSONR[A])(f: (A) => B): JSONR[B] = new JSONR[B] {
      override def read(json: JValue): Result[B] = {
        fa.read(json) map f
      }
    }
  }

  object JSONR extends LabelledTypeClassCompanion[JSONR] {

    object typeClass extends LabelledTypeClass[JSONR] {
      def emptyProduct = new JSONR[HNil] {
        def read(value: JValue) = HNil.successNel
      }

      def product[F, T <: HList](name: String, sh: JSONR[F], st: JSONR[T]) = new JSONR[F :: T] {
        def read(v: JValue): Result[F :: T] = {

          val head: Result[F] = v match {
            case x:JObject => (x \ name).validate[F](sh)
            case _ => Fail(name, s"Could not read value")
          }

          val tail: Result[T] = st.read(v)

          (head |@| tail) { case (f, t) => f :: t }
        }
      }

      def project[F, G](instance: => JSONR[G], to: F => G, from: G => F) = new JSONR[F] {
        def read(v: JValue): Result[F] = instance.read(v) map from
      }

      // TODO too specific, maybe remove
      override def coproduct[L, R <: Coproduct](name: String, cl: => JSONR[L], cr: => JSONR[R]): JSONR[:+:[L, R]] = {
        new JSONR[L :+: R] {
          override def read(json: JValue): Result[L :+: R] = {
            (json \ "_tpe").validate[String] match {
              case Success(tpe) =>
                if (tpe == name) cl.read(json) map (x => Inl.apply[L, R](x))
                else cr.read(json) map (x => Inr.apply[L, R](x))
              case _ =>
                sys.error("no _tpe flag, not possible to discriminate types")
            }
          }
        }
      }

      override def emptyCoproduct: JSONR[CNil] = {
        new JSONR[CNil] {
          override def read(json: JValue): Result[CNil] = {
            Fail("", s"Could not read from $json")
          }
        }
      }
    }

  }


  // TODO make optional
  object JSONW extends LabelledTypeClassCompanion[JSONW] {

    object typeClass extends LabelledTypeClass[JSONW] {
      def emptyProduct = new JSONW[HNil] {
        override def write(a: HNil) = JObject()
      }

      def product[F, T <: HList](name: String, sh: JSONW[F], st: JSONW[T]) = new JSONW[F :: T] {
        override def write(value: F :: T): JValue = {

          // is a value
          val head: JValue = sh.write(value.head)

          // is a JObject
          val tail: JValue = st.write(value.tail)

          tail match {
            case x: JObject => JObject((name -> head) :: x.obj)
            case _ => JObject((name -> head))
          }
        }
      }

      override def project[F, G](instance: => JSONW[G], to: (F) => G, from: (G) => F): JSONW[F] = {
        new JSONW[F] {
          def write(f: F): JValue = instance.write(to(f))
        }
      }

      override def coproduct[L, R <: Coproduct](name: String, cl: => JSONW[L], cr: => JSONW[R]): JSONW[L :+: R] = {
        new JSONW[L :+: R] {
          override def write(value: L :+: R): JValue = value match {
            case Inl(l) => cl.write(l)
            case Inr(r) => cr.write(r)
          }
        }
      }

      override def emptyCoproduct: JSONW[CNil] = {
        new JSONW[CNil] {
          override def write(value: CNil): JValue = JNothing
        }
      }
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

object JsonScalaz extends Types with Lifting with Base with Dsl with ReadExt with WriteExt with Tuples
