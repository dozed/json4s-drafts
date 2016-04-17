package org.json4s.ext.scalaz

import org.json4s._
import scalaz._, Scalaz._

trait TupleMaps extends Dsl { self: Types =>

   def json2[R, A:JSON, B:JSON](key1: String, key2: String)(f: R => Option[(A, B)], g: (A, B) => R): JSON[R] = {
    JSON.instance[R](
      jsonr2(key1, key2)(g),
      jsonw2(key1, key2)(f)
    )
  }

  def json3[R, A:JSON, B:JSON, C:JSON](key1: String, key2: String, key3: String)(f: R => Option[(A, B, C)], g: (A, B, C) => R): JSON[R] = {
    JSON.instance[R](
      jsonr3(key1, key2, key3)(g),
      jsonw3(key1, key2, key3)(f)
    )
  }

  def json4[R, A:JSON, B:JSON, C:JSON, D:JSON](key1: String, key2: String, key3: String, key4: String)(f: R => Option[(A, B, C, D)], g: (A, B, C, D) => R): JSON[R] = {
    JSON.instance[R](
      jsonr4(key1, key2, key3, key4)(g),
      jsonw4(key1, key2, key3, key4)(f)
    )
  }



  def jsonr2[R, A, B](key1: String, key2: String)(f: (A, B) => R)(implicit jsonA: JSON[A], jsonB: JSON[B]): JSONR[R] = {
    JSON.read { json =>
      ((json \ key1).validate[A] |@| (json \ key2).validate[B]).apply(f)
    }
  }

  def jsonr3[R, A, B, C](key1: String, key2: String, key3: String)(f: (A, B, C) => R)(implicit jsonA: JSON[A], jsonB: JSON[B], jsonC: JSON[C]): JSONR[R] = new JSONR[R] {
    override def read(json: JValue): Result[R] = {
      ((json \ key1).validate[A] |@| (json \ key2).validate[B] |@| (json \ key3).validate[C]).apply(f)
    }
  }

  def jsonr4[R, A, B, C, D](key1: String, key2: String, key3: String, key4: String)(f: (A, B, C, D) => R)(implicit jsonA: JSON[A], jsonB: JSON[B], jsonC: JSON[C], jsonD: JSON[D]): JSONR[R] = new JSONR[R] {
    override def read(json: JValue): Result[R] = {
      ((json \ key1).validate[A] |@| (json \ key2).validate[B] |@| (json \ key3).validate[C] |@| (json \ key4).validate[D]).apply(f)
    }
  }



  def jsonw2[R, A, B](key1: String, key2: String)(f: R => Option[(A, B)])(implicit jsonA: JSON[A], jsonB: JSON[B]): JSONW[R] = {
    JSON.write[R] { r =>
      val (a, b) = f(r).get
      (key1 -> jsonA.write(a)) ~ (key2 -> jsonB.write(b))
    }
  }

  def jsonw3[R, A, B, C](key1: String, key2: String, key3: String)(f: R => Option[(A, B, C)])(implicit jsonA: JSON[A], jsonB: JSON[B], jsonC: JSON[C]): JSONW[R] = new JSONW[R] {
    override def write(value: R): JValue = {
      val (a, b, c) = f(value).get
      (key1 -> jsonA.write(a)) ~ (key2 -> jsonB.write(b)) ~ (key3 -> jsonC.write(c))
    }
  }

  def jsonw4[R, A, B, C, D](key1: String, key2: String, key3: String, key4: String)(f: R => Option[(A, B, C, D)])(implicit jsonA: JSON[A], jsonB: JSON[B], jsonC: JSON[C], jsonD: JSON[D]): JSONW[R] = new JSONW[R] {
    override def write(value: R): JValue = {
      val (a, b, c, d) = f(value).get
      (key1 -> jsonA.write(a)) ~ (key2 -> jsonB.write(b)) ~ (key3 -> jsonC.write(c)) ~ (key4 -> jsonD.write(d))
    }
  }

}
