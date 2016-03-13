package drafts

object ReadExt {

  import org.json4s._
  import org.json4s.scalaz.JsonScalaz._

  import _root_.scalaz._, Scalaz._
  import shapeless.{:+:, Coproduct, _}

  implicit class JArrayOps(j: JArray) {
    def head: JValue = j.children.head
    def tail: JValue = JArray(j.children.tail)
  }

  implicit class JValueSyntax(value: JValue) {
    def validate[A: JSONR]: ValidationNel[Error, A] = implicitly[JSONR[A]].read(value)
    def read[A: JSONR]: Error \/ A = implicitly[JSONR[A]].read(value).disjunction.leftMap(_.head)
  }

  def read[A](f: JValue => Result[A]) = new JSONR[A] {
    def read(json: JValue) = f(json)
  }

  def readE[A](f: JValue => Error \/ A) = new JSONR[A] {
    def read(json: JValue) = f(json).validationNel
  }

  def validate2[A:JSONR](json: JValue): Result[A] = implicitly[JSONR[A]].read(json)

  def readR[A](r: JSONR[A]): JSONR[A] = r

  def fieldT[A:JSONR](f: JValue => JValue): JValue => Result[A] = { json =>
    implicitly[JSONR[A]].read(f(json))
  }

  object JSONRExt extends LabelledTypeClassCompanion[JSONR] {

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

}
