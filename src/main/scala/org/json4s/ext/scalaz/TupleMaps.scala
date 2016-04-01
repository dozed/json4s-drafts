package org.json4s.ext.scalaz

import org.json4s._
import scalaz._, Scalaz._

trait TupleMaps extends Dsl { self: Types =>

   def json2[R, A:JSON, B:JSON](key1: String, key2: String)(f1: R => Option[(A, B)], f2: (A, B) => R): JSON[R] = {
    JSON[R](
      jsonr2(key1, key2)(f2),
      jsonw2(key1, key2)(f1)
    )
  }

  def json3[R, A:JSON, B:JSON, C:JSON](key1: String, key2: String, key3: String)(f1: R => Option[(A, B, C)], f2: (A, B, C) => R): JSON[R] = {
    JSON[R](
      jsonr3(key1, key2, key3)(f2),
      jsonw3(key1, key2, key3)(f1)
    )
  }

  def jsonr2[R, A, B](key1: String, key2: String)(f2: (A, B) => R)(implicit jsonA: JSON[A], jsonB: JSON[B]): JSONR[R] = {
    JSON.read { json =>
      ((json \ key1).validate[A] |@| (json \ key2).validate[B]).apply(f2)
    }
  }

  def jsonw2[R, A, B](key1: String, key2: String)(f1: R => Option[(A, B)])(implicit jsonA: JSON[A], jsonB: JSON[B]): JSONW[R] = {
    JSON.write[R] { r =>
      val (a, b) = f1(r).get
      (key1 -> jsonA.write(a)) ~ (key2 -> jsonB.write(b))
    }
  }

  def jsonr3[R, A, B, C](key1: String, key2: String, key3: String)(f2: (A, B, C) => R)(implicit jsonA: JSON[A], jsonB: JSON[B], jsonC: JSON[C]): JSONR[R] = new JSONR[R] {
    override def read(json: JValue): Result[R] = {
      ((json \ key1).validate[A] |@| (json \ key2).validate[B] |@| (json \ key3).validate[C]).apply(f2)
    }
  }

  def jsonw3[R, A, B, C](key1: String, key2: String, key3: String)(f1: R => Option[(A, B, C)])(implicit jsonA: JSON[A], jsonB: JSON[B], jsonC: JSON[C]): JSONW[R] = new JSONW[R] {
    override def write(value: R): JValue = {
      val (a, b, c) = f1(value).get
      (key1 -> jsonA.write(a)) ~ (key2 -> jsonB.write(b)) ~ (key3 -> jsonC.write(c))
    }
  }

}
