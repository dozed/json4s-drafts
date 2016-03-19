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

  implicit def jvalueWriter[A <: JValue] = write[A](identity)

  // direct writers
  implicit class WriterOps[A](a: A) {
    def toJson(implicit w: JSONW[A]): JValue = w.write(a)

    def toJson[C](c: C)(implicit w: JSONW[JsonWriterContext[C, A]]) = w.write(JsonWriterContext(c, a))
  }


  import scala.language.experimental.macros

  def typeTagGen[A]: A => String = macro Macros.typeTagGenImpl[A]

  def writerGen[A]: JSONW[A] = macro Macros.writerGenImpl[A]





}
