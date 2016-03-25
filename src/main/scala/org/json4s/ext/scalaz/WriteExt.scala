package org.json4s.ext.scalaz

import org.json4s._

trait WriteExt { self: Types =>

  // specific things:
  // - type tags
  // - context-dependent writer, write an A with context C (tagged tuple2)

  implicit def jvalueWriter[A <: JValue] = JSON.write[A](identity)

  implicit class WriterOps[A](a: A) {
    def toJson(implicit w: JSONW[A]): JValue = w.write(a)

    def toJson[C](c: C)(implicit w: JSONW[JSONWContext[C, A]]) = w.write(JSONWContext(c, a))
  }

  implicit class JsonWriterOps[A](w: JSONW[A]) {
    def withTag(tag: A => String, name: String = "type"): JSONW[A] = JSON.write[A] { a =>
      w.write(a) match {
        case jobj: JObject => JObject(name -> JString(tag(a)) :: jobj.obj)
        case x => x
      }
    }
  }

}
