package org.json4s.ext.scalaz

import org.json4s._
import org.json4s.ext.scalaz.JValueExts._
import shapeless.newtype._
import shapeless.labelled._
import shapeless.{Coproduct, :+:, _}
import shapeless.ops.coproduct.Inject

import scalaz._, Scalaz._

trait JsonShapeless { self: Types =>

  case class Thing[A](value: A)

  def deriveJSONR[A](implicit readA: Thing[JSONR[A]]): JSONR[A] = readA.value
  def deriveJSONW[A](implicit writeA: Thing[JSONW[A]]): JSONW[A] = writeA.value
  def deriveJSON[A](implicit readA: Thing[JSONR[A]], writeA: Thing[JSONW[A]]): JSON[A] = new JSON[A] {
    override def write(value: A): JValue = writeA.value.write(value)
    override def read(json: JValue): Result[A] = readA.value.read(json)
  }




  // JSONW for
  // - HNil
  // - HCons: H :: T
  // - HCons for field HLists: FieldType[K, H] :: T
  // - CNil
  // - CCons: H :+: T
  //
  // basically those can provide combinations for elementar instances when requested by implicit resolution, e.g. JSONW[H] + JSONW[T] => JSONW[H :: T]

  implicit val writeHNil: JSONW[HNil] =
    new JSONW[HNil] {
      final def write(a: HNil): JValue = JNothing
    }

  implicit def writeHCons[H, T <: HList](implicit writeHead: Lazy[JSONW[H]], writeTail: Lazy[JSONW[T]]): JSONW[H :: T] =
    new JSONW[H :: T] {
      final def write(ab: H :: T): JValue = {
        JArray(List(writeHead.value.write(ab.head))) ++ writeTail.value.write(ab.tail)
      }
    }

  implicit def writeFieldHCons[K <: Symbol, H, T <: HList](implicit key: Witness.Aux[K],
                                                           writeHead: Lazy[JSONW[H]],
                                                           writeTail: Lazy[JSONW[T]]
                                                          ): JSONW[FieldType[K, H] :: T] =
    new JSONW[FieldType[K, H] :: T] {
      def write(a: FieldType[K, H] :: T): JValue = a match {
        case h :: t =>

          // is a value
          val head: JValue = writeHead.value.write(h)

          // is a JObject
          val tail: JValue = writeTail.value.write(t)

          tail match {
            case x: JObject => JObject((key.value.name -> head) :: x.obj)
            case _ => JObject(key.value.name -> head)
          }
      }
    }

  implicit lazy val writeCNil: JSONW[CNil] = new JSONW[CNil] {
    override def write(value: CNil): JValue = JNothing
  }

  implicit def writeCCons[H, T <: Coproduct](implicit writeHead: Lazy[JSONW[H]], writeTail: Lazy[JSONW[T]]): JSONW[H :+: T] = {
    new JSONW[H :+: T] {
      override def write(value: H :+: T): JValue = value match {
        case Inl(l) => writeHead.value.write(l)
        case Inr(r) => writeTail.value.write(r)
      }
    }
  }




  // same for JSONR

  implicit val readHNil: JSONR[HNil] =
    new JSONR[HNil] {
      def read(c: JValue): Result[HNil] = HNil.successNel
    }

  implicit def readHCons[H, T <: HList](implicit readHead: Lazy[JSONR[H]], readTail: Lazy[JSONR[T]]): JSONR[H :: T] =
    new JSONR[H :: T] {
      final def read(json: JValue): Result[H :: T] = {
        json.asJArray.fold({
          UnexpectedJSONError(json, classOf[JArray]).asInstanceOf[Error].failureNel[H :: T]
        })(x =>{
          {
            readHead.value.read(x.head) |@| readTail.value.read(x.tail)
          }.apply[H :: T]((h, t) => h :: t)
        })
      }
    }

  implicit final def readFieldHCons[K <: Symbol, H, T <: HList](implicit key: Witness.Aux[K],
                                                                readHead: Lazy[JSONR[H]],
                                                                readTail: Lazy[JSONR[T]]
                                                                ): JSONR[FieldType[K, H] :: T] =
    new JSONR[FieldType[K, H] :: T] {
      def read(json: JValue): Result[FieldType[K, H] :: T] = {

        val head: Result[H] = json match {
          case obj:JObject => (obj \ key.value.name).validate[H](readHead.value)
          case _ => Fail(key.value.name, s"Could not read value", List(json))
        }

        val tail: Result[T] = readTail.value.read(json)

        (head |@| tail) { case (f, t) => labelled.field[K](f) :: t }

      }
    }

  implicit lazy val readCNil: JSONR[CNil] = new JSONR[CNil] {
    override def read(json: JValue): Result[CNil] = Fail("invalid_json_for_coproduct", "no element of this coproduct matched the json")
  }


  implicit def readCCons[H, T <: Coproduct](implicit readHead: Lazy[JSONR[H]], readTail: Lazy[JSONR[T]]): JSONR[H :+: T] = {
    new JSONR[H :+: T] {
      override def read(json: JValue): Result[H :+: T] = {
        readHead.value.read(json).map(x => Inl(x)) |||
          readTail.value.read(json).map(x => Inr(x))
      }
    }
  }





  // a LabelledGeneric can be derived to Thing[JSONR[A]]

  // LabelledGeneric representing a labelled HList (fields of a case class, product)
  implicit def productRead[A, R <: HList](implicit gen: LabelledGeneric.Aux[A, R], readR: Lazy[JSONR[R]]): Thing[JSONR[A]] =
    Thing(readR.value.map(gen.from))

  implicit def productWrite[A, R <: HList](implicit gen: LabelledGeneric.Aux[A, R], writeR: Lazy[JSONW[R]]): Thing[JSONW[A]] =
    Thing(writeR.value.contramap(gen.to))

  // LabelledGeneric representing a labelled Coproduct (element of an inheritance hierarchy, sum)
  implicit def adtWrite[A, R <: Coproduct](implicit gen: LabelledGeneric.Aux[A, R], writeR: Lazy[JSONW[R]]): Thing[JSONW[A]] =
    Thing(writeR.value.contramap(gen.to))

  implicit def adtRead[A, R <: Coproduct](implicit gen: LabelledGeneric.Aux[A, R], readR: Lazy[JSONR[R]]): Thing[JSONR[A]] =
    Thing(readR.value.map(gen.from))

  // we need also those
  implicit def adtThingWrite[A, R <: Coproduct](implicit gen: LabelledGeneric.Aux[A, R], writeR: Lazy[Thing[JSONW[R]]]): Thing[JSONW[A]] =
    Thing(writeR.value.value.contramap(gen.to))

  implicit def adtThingRead[A, R <: Coproduct](implicit gen: LabelledGeneric.Aux[A, R], readR: Lazy[Thing[JSONR[R]]]): Thing[JSONR[A]] =
    Thing(readR.value.value.map(gen.from))

  // coproduct with nested encoding
  implicit def coproductRead[K <: Symbol, H, T <: Coproduct](implicit key: Witness.Aux[K], readHead: Lazy[Thing[JSONR[H]]], readTail: Lazy[JSONR[T]]): Thing[JSONR[FieldType[K, H] :+: T]] =
    Thing[JSONR[FieldType[K, H] :+: T]] {
      new JSONR[FieldType[K, H] :+: T] {
        def read(json: JValue): Result[FieldType[K, H] :+: T] = {
          (json \ key.value.name).validate[JObject].fold(
            _ => readTail.value.read(json) map (x => Inr.apply[FieldType[K, H], T](x)),
            obj => readHead.value.value.read(obj).map(x => Inl.apply[FieldType[K, H], T](labelled.field[K](x)))
          )
        }
      }
    }

  implicit def coproductWrite[K <: Symbol, H, T <: Coproduct](implicit key: Witness.Aux[K], writeHead: Lazy[Thing[JSONW[H]]], writeTail: Lazy[JSONW[T]]): Thing[JSONW[FieldType[K, H] :+: T]] =
    Thing[JSONW[FieldType[K, H] :+: T]] {
      new JSONW[FieldType[K, H] :+: T] {
        def write(a: FieldType[K, H] :+: T): JValue = a match {
          case Inl(h) => JObject(key.value.name -> writeHead.value.value.write(h))
          case Inr(t) => writeTail.value.write(t)
        }
      }
    }



  object auto {
    // implicit def thingFoo[A, F[_]](implicit fa: Thing[F[A]]): F[A] = fa.value.value
    implicit def thingFoo1[A](implicit fa: Thing[JSONR[A]]): JSONR[A] = fa.value
    implicit def thingFoo2[A](implicit fa: Thing[JSONW[A]]): JSONW[A] = fa.value
  }


  // syntax for a JSONR[A] where A is a part of a Coproduct

  implicit class JSONRShapelessExt(json: JValue) {
    def validateC[A:JSONR, C <: Coproduct](implicit inj: Inject[C, A]): Result[C] = {
      implicitly[JSONR[A]].read(json).map(t => Coproduct[C](t))
    }
  }


  // JSON for a Newtype

  implicit def writeNewtype[A, Ops <: { def value: A }](implicit write0: Lazy[JSONW[A]], mk: A => Ops): JSONW[Newtype[A, Ops]] = {
    write0.value.contramap[Newtype[A, Ops]](a => a.value)
  }

  implicit def readNewtype[A, Ops](implicit read0: JSONR[A]): JSONR[Newtype[A, Ops]] = {
    read0.map((x: A) => newtype[A, Ops](x))
  }


}
