package org.json4s.ext.scalaz

import org.json4s._

trait Dsl { self: Types =>

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

}
