package net.virtualvoid.jsontypes

import spray.json.JsArray
import spray.json.JsObject
import spray.json.JsString
import spray.json.JsValue

sealed trait JsType
object JsType {
  type Alternatives = Set[JsType]

  final case class ArrayOf(structure: JsType) extends JsType
  final case class OneOf(alternatives: Alternatives) extends JsType
  final case class ObjectOf(fields: Map[String, JsType]) extends JsType

  sealed trait PrimitiveType extends JsType

  case object StringType extends PrimitiveType
  case object NumberType extends PrimitiveType
  case object BooleanType extends PrimitiveType

  case object NullType extends JsType

  case class ValueOrNull(valueStructure: JsType) extends JsType

  case object Missing extends JsType

  def toJson(s: JsType): JsValue = s match {
    case ArrayOf(els) => JsArray(toJson(els))
    case ObjectOf(fields) =>
      val entries = fields.mapValues(toJson)
      JsObject(entries)
    case OneOf(els) =>
      if (els.size == 2 && els.contains(Missing))
        JsObject("optional" -> toJson((els - Missing).head))
      else
        JsObject("oneOf" -> JsArray(els.map(toJson).toVector))
    case ValueOrNull(valueStructure) =>
      JsObject("optional" -> toJson(valueStructure))
    case x => JsString(x.toString)
  }
}