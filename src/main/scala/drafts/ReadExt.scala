package drafts

object ReadExt {

  import org.json4s._
  import org.json4s.scalaz.JsonScalaz._

  import _root_.scalaz._, Scalaz._

  implicit class JArrayOps(j: JArray) {
    def head: JValue = j.children.head
    def tail: JValue = JArray(j.children.tail)
  }

  implicit class JvalueOps(value: JValue) {
    def validate[A: JSONR]: ValidationNel[Error, A] = implicitly[JSONR[A]].read(value)
    def read[A: JSONR]: Error \/ A = implicitly[JSONR[A]].read(value).disjunction.leftMap(_.head)
  }

  def validate1[A](f: JValue => Result[A]) = new JSONR[A] {
    def read(json: JValue) = f(json)
  }

  def read[A](f: JValue => Error \/ A) = new JSONR[A] {
    def read(json: JValue) = f(json).validationNel
  }

  def validate2[A:JSONR](json: JValue): Result[A] = implicitly[JSONR[A]].read(json)

}
