package org.json4s.ext.scalaz

import org.json4s._

trait Dsl { self: Types =>

  // implicit def asJValue[A:JSONW](a: A): JValue = implicitly[JSONW[A]].write(a)

  implicit def pair2JObject[A:JSONW](t: (String, A)): JObject = JObject((t._1, implicitly[JSONW[A]].write(t._2)))

  implicit def pair2Assoc[A:JSONW](t: (String, A)): JsonObjectAssoc = new JsonObjectAssoc(JObject((t._1, implicitly[JSONW[A]].write(t._2))))

  implicit class JsonObjectAssoc(left: JObject) {
    def ~[A:JSONW](right: (String, A)): JObject = this.~(JObject((right._1, implicitly[JSONW[A]].write(right._2))))

    def ~(right: JValue): JObject = {
      right match {
        case jobj: JObject => JObject(left.obj ::: jobj.obj)
        case _ => left
      }
    }
  }

}
