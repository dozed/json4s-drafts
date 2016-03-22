package org.json4s.ext.scalaz

import org.json4s._

trait WriteExt { self: Types =>

  // specific things:
  // - type tags
  // - context-dependent writer, write an A with context C (tagged tuple2)

  implicit def jvalueWriter[A <: JValue] = write[A](identity)

  implicit class WriterOps[A](a: A) {
    def toJson(implicit w: JSONW[A]): JValue = w.write(a)

    def toJson[C](c: C)(implicit w: JSONW[JsonWriterContext[C, A]]) = w.write(JsonWriterContext(c, a))
  }

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
      w.write(a) match {
        case jobj: JObject => JObject(name -> JString(tag(a)) :: jobj.obj)
        case x => x
      }
    }
  }

}
