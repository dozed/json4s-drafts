package org.json4s.ext.scalaz

import org.json4s._
import org.json4s.ext.scalaz.JValueExts._
import shapeless.newtype._
import shapeless.labelled._
import shapeless.{Coproduct, :+:, _}
import shapeless.ops.coproduct.Inject

import scalaz._, Scalaz._

trait JsonShapeless { self: Types =>

  implicit class JSONRShapelessOps(json: JValue) {
    def validateC[A:JSONR, C <: Coproduct](implicit inj: Inject[C, A]): Result[C] = {
      implicitly[JSONR[A]].read(json).map(t => Coproduct[C](t))
    }
  }


  // JSON for a Newtype

  implicit def readNewtype[A, Ops](implicit read0: JSONR[A]): JSONR[Newtype[A, Ops]] = {
    read0.map((x: A) => newtype[A, Ops](x))
  }

  implicit def writeNewtype[A, Ops <: { def value: A }](implicit write0: Lazy[JSONW[A]], mk: A => Ops): JSONW[Newtype[A, Ops]] = {
    write0.value.contramap[Newtype[A, Ops]](a => a.value)
  }


  // JSON for HList
  implicit val readHNil: JSONR[HNil] =
    new JSONR[HNil] {
      def read(c: JValue): Result[HNil] = HNil.successNel
    }


  implicit val writeHNil: JSONW[HNil] =
    new JSONW[HNil] {
      final def write(a: HNil): JValue = JNothing
    }

  // JSONW[H] + JSONW[T] => JSONW[H :: T]
  implicit def writeHCons[H, T <: HList](implicit writeHead: Lazy[JSONW[H]], writeTail: Lazy[JSONW[T]]): JSONW[H :: T] =
    new JSONW[H :: T] {
      final def write(ab: H :: T): JValue = {
        JArray(List(writeHead.value.write(ab.head))) ++ writeTail.value.write(ab.tail)
      }
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


  // JSON for a Coproduct

  implicit lazy val writeCNil: JSONW[CNil] = new JSONW[CNil] {
    override def write(value: CNil): JValue = JNothing
  }

  implicit lazy val readCNil: JSONR[CNil] = new JSONR[CNil] {
    override def read(json: JValue): Result[CNil] = Fail("invalid_json_for_coproduct", "no element of this coproduct matched the json")
  }

  implicit def writeCCons[H, T <: Coproduct](implicit writeHead: Lazy[JSONW[H]], writeTail: Lazy[JSONW[T]]): JSONW[H :+: T] = {
    new JSONW[H :+: T] {
      override def write(value: H :+: T): JValue = value match {
        case Inl(l) => writeHead.value.write(l)
        case Inr(r) => writeTail.value.write(r)
      }
    }
  }

  implicit def readCCons[H, T <: Coproduct](implicit readHead: Lazy[JSONR[H]], readTail: Lazy[JSONR[T]]): JSONR[H :+: T] = {
    new JSONR[H :+: T] {
      override def read(json: JValue): Result[H :+: T] = {
        readHead.value.read(json).map(x => Inl(x)) |||
          readTail.value.read(json).map(x => Inr(x))
      }
    }
  }

  // Labelled HList (Record)

  implicit def writeLabelledHList[K <: Symbol, H, T <: HList](implicit key: Witness.Aux[K],
                                                                readHead: Lazy[JSONW[H]],
                                                                readTail: Lazy[JSONW[T]]
                                                              ): JSONW[FieldType[K, H] :: T] =
    new JSONW[FieldType[K, H] :: T] {
      def write(a: FieldType[K, H] :: T): JValue = a match {
        case h :: t =>

          // is a value
          val head: JValue = readHead.value.write(h)

          // is a JObject
          val tail: JValue = readTail.value.write(t)

          tail match {
            case x: JObject => JObject((key.value.name -> head) :: x.obj)
            case _ => JObject(key.value.name -> head)
          }
      }
    }



  implicit final def readLabelledHList[K <: Symbol, H, T <: HList](implicit key: Witness.Aux[K],
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



  // JSON for LabelledGeneric product and coproduct
  // inspired from: https://github.com/travisbrown/circe/tree/master/generic/shared/src/main/scala/io/circe/generic
  object auto {

    implicit def writeCoproduct[K <: Symbol, H, T <: Coproduct](implicit key: Witness.Aux[K], readHead: Lazy[JSONW[H]], readTail: Lazy[JSONW[T]]): JSONW[FieldType[K, H] :+: T] =
      new JSONW[FieldType[K, H] :+: T] {
        def write(a: FieldType[K, H] :+: T): JValue = a match {
          case Inl(h) => JObject(
            key.value.name -> readHead.value.write(h)
          )
          case Inr(t) => readTail.value.write(t)
        }
      }

    implicit def writeAdt[A, R <: Coproduct](implicit gen: LabelledGeneric.Aux[A, R], write0: Lazy[JSONW[R]]): JSONW[A] = new JSONW[A] {
      def write(a: A): JValue = write0.value.write(gen.to(a))
    }


    implicit def writeCaseClass[A, R <: HList](implicit gen: LabelledGeneric.Aux[A, R], write0: Lazy[JSONW[R]]): JSONW[A] = new JSONW[A] {
      def write(a: A): JValue = write0.value.write(gen.to(a))
    }

    // try to read the coproduct element H with key
    implicit final def readCoproduct[K <: Symbol, H, T <: Coproduct](implicit key: Witness.Aux[K], readHead: Lazy[JSONR[H]], readTail: Lazy[JSONR[T]]): JSONR[FieldType[K, H] :+: T] =
      new JSONR[FieldType[K, H] :+: T] {
        def read(json: JValue): Result[FieldType[K, H] :+: T] = {
          (json \ key.value.name).validate[JObject] match {
            case Success(obj) =>
              readHead.value.read(obj)
                .map(x => Inl.apply[FieldType[K, H], T](labelled.field[K](x)))
            case _ =>
              readTail.value.read(json) map (x => Inr.apply[FieldType[K, H], T](x))

          }
        }
      }

    implicit final def readAdt[A, R <: Coproduct](implicit gen: LabelledGeneric.Aux[A, R], read0: Lazy[JSONR[R]]): JSONR[A] = new JSONR[A] {
      def read(c: JValue): Result[A] = read0.value.read(c).map(gen.from)
    }

    implicit final def readCaseClass[A, R <: HList](implicit gen: LabelledGeneric.Aux[A, R], read0: Lazy[JSONR[R]]): JSONR[A] = new JSONR[A] {
      def read(c: JValue): Result[A] = read0.value.read(c).map(gen.from)
    }

  }


}
