package org.json4s.ext.scalaz

import org.json4s._
import shapeless.newtype._
import shapeless.ops.coproduct.Inject
import shapeless.{Coproduct, :+:, _}

import scalaz._, Scalaz._

trait JsonShapeless { self: Types =>

  // JSON for a Newtype
  implicit def readNewtype[A, Ops](implicit read: JSONR[A]): JSONR[Newtype[A, Ops]] = {
    read.map((x: A) => newtype[A, Ops](x))
  }

  implicit def writeNewtype[A, Ops <: { def value: A }](implicit write: Lazy[JSONW[A]], mk: A => Ops): JSONW[Newtype[A, Ops]] = {
    write.value.contramap[Newtype[A, Ops]](a => a.value)
  }

  // JSON for a Coproduct
  implicit lazy val writeCNil: JSONW[CNil] = new JSONW[CNil] {
    override def write(value: CNil): JValue = JNothing
  }

  implicit def writeCCons[L, R <: Coproduct](implicit cl: Lazy[JSONW[L]], cr: Lazy[JSONW[R]]): JSONW[L :+: R] = {
    new JSONW[L :+: R] {
      override def write(value: L :+: R): JValue = value match {
        case Inl(l) => cl.value.write(l)
        case Inr(r) => cr.value.write(r)
      }
    }
  }

}
