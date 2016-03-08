
object HlistExample extends App {

  import org.json4s._
  import org.json4s.jackson._
  import org.json4s.scalaz.JsonScalaz._
  import shapeless._
  import _root_.scalaz._, Scalaz._

  import drafts.ReadExt._




  object ProductReader extends ProductTypeClassCompanion[JSONR] {
    override val typeClass: ProductTypeClass[JSONR] = new ProductTypeClass[JSONR] {
      override def product[H, T <: HList](ch: JSONR[H], ct: JSONR[T]): JSONR[::[H, T]] = new JSONR[H :: T] {
        override def read(value: JValue): Result[::[H, T]] = {
          value match {

            case x:JArray =>
              {
                ch.read(x.head) |@|  ct.read(x.tail)
              }.apply((h, t) => h :: t)

            case x =>
              UnexpectedJSONError(x, classOf[JArray]).asInstanceOf[Error].failureNel

          }
        }
      }

      override def emptyProduct: JSONR[HNil] = jsonr[HNil](_ => HNil.successNel)

      override def project[F, G](instance: => JSONR[G], to: (F) => G, from: (G) => F): JSONR[F] = new JSONR[F] {
        override def read(value: JValue): Result[F] = {
          instance.read(value) map (g => from(g))
        }
      }
    }
  }

  import ProductReader._

  implicitly[JSONR[String]]
  implicitly[JSONR[List[String]]]
  implicitly[JSONR[String :: Int :: HNil]]
  val json = parseJson("""["hey", 42]""")

  val x = json.read[String :: Int :: HNil]

  println(x)
  // ReadSuccess(hey :: 42 :: HNil)

}
