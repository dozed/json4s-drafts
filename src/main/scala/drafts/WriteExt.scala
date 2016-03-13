package drafts

object WriteExt {

  import org.json4s._
  import org.json4s.scalaz.JsonScalaz._

  // specific things:
  // - type tags
  // - context-dependent writer, write an A with context C (tagged tuple2)

  case class JsonWriterContext[C, A](a: (C, A))

  object write {

    def apply[A](f: A => JValue): JSONW[A] = new JSONW[A] {
      override def write(a: A): JValue = f(a)
    }

    def nil[A] = write[A](_ => JNothing)

    def context[C, A](f: (C, A) => JValue): JSONW[JsonWriterContext[C, A]] = write[JsonWriterContext[C, A]](ca => f.tupled(ca.a))

  }

  implicit class JsonWriterOps[A](w: JSONW[A]) {
    def withTag(tag: A => String, name: String = "type"): JSONW[A] = write[A] { a =>
      JObject(name -> JString(tag(a))) ~ w.write(a)
    }
  }

  // mini DSL
  implicit def pair2JObject[A](t: (String, A))(implicit w: JSONW[A]): JObject = JObject((t._1, w.write(t._2)))

  implicit def pair2Assoc[A](t: (String, A))(implicit w: JSONW[A]): JsonObjectAssoc = new JsonObjectAssoc(JObject((t._1, w.write(t._2))))

  implicit class JsonObjectAssoc(left: JObject) {
    def ~[A](right: (String, A))(implicit w: JSONW[A]): JObject = this.~(JObject((right._1, w.write(right._2))))

    def ~(right: JValue): JObject = {
      right match {
        case jobj: JObject => JObject(left.obj ::: jobj.obj)
        case _ => left
      }
    }
  }

  // direct writers
  implicit class WriterOps[A](a: A) {
    def toJson(implicit w: JSONW[A]): JValue = w.write(a)

    def toJson[C](c: C)(implicit w: JSONW[JsonWriterContext[C, A]]) = w.write(JsonWriterContext(c, a))
  }


  import scala.language.experimental.macros

  def typeTagGen[A]: A => String = macro Macros.typeTagGenImpl[A]

  def writerGen[A]: JSONW[A] = macro Macros.writerGenImpl[A]


  import shapeless._

  object JSONWExt extends LabelledProductTypeClassCompanion[JSONW] {

    // todo LabelledTypeClass
    object typeClass extends LabelledProductTypeClass[JSONW] {
      def emptyProduct = new JSONW[HNil] {
        override def write(a: HNil) = JObject()
      }

      // traversing a product
      // - write field "name" of some object to a field, with tail of other fields
      // - tail has no acess to the other fields
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
    }

  }


}
