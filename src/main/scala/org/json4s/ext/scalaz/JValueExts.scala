package org.json4s.ext.scalaz

import org.json4s._

object JValueExts extends JValueExts

trait JValueExts {

  implicit class JArrayExt(v: JArray) {
    def head: JValue = v.children.head
    def tail: JValue = JArray(v.children.tail)
  }

  implicit class JValueExt(v: JValue) {
    def isDefined: Boolean = !isEmpty

    def isEmpty = v match {
      case JNothing => true
      case JNull => true
      case _ => false
    }

    def asJArray: Option[JArray] = v match {
      case arr: JArray => Some(arr)
      case _ => None
    }

    def asJObject: Option[JObject] = v match {
      case obj: JObject => Some(obj)
      case _ => None
    }

  }

}
