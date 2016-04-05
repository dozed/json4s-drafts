package org.json4s.ext.scalaz

import org.json4s._
import org.json4s.jackson.compactJson

object JValueExts extends JValueExts

trait JValueExts {

  implicit class JArrayExt(json: JArray) {
    def head: JValue = json.children.head
    def tail: JArray = JArray(json.children.tail)

    def size: Int = json.children.size

    def map(f: JValue => JValue): JArray = {
      JArray(json.children.map(f))
    }

    def flatMap(f: JValue => List[JValue]): JArray = {
      JArray(json.children.flatMap(x => f(x).toList))
    }
  }

  implicit class JValueExt(json: JValue) {
    def isDefined: Boolean = !isEmpty

    def isEmpty = json match {
      case JNothing => true
      case JNull => true
      case _ => false
    }

    def asJArray: Option[JArray] = json match {
      case arr: JArray => Some(arr)
      case _ => None
    }

    def asJObject: Option[JObject] = json match {
      case obj: JObject => Some(obj)
      case _ => None
    }

    def nospaces: String = compactJson(json)

  }

}
