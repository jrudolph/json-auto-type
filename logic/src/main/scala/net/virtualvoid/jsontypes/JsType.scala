package net.virtualvoid.jsontypes

import scala.language.postfixOps

import spray.json.JsArray
import spray.json.JsObject
import spray.json.JsString
import spray.json.JsValue

sealed trait JsType {
  def widen: JsType = JsType.widen(this)
}
object JsType {
  type Alternatives = Set[JsType]

  final case class Constant(value: JsValue, jsType: JsType) extends JsType

  final case class ArrayOf(structure: JsType) extends JsType
  final case class OneOf(alternatives: Alternatives) extends JsType {
    require(alternatives.size > 1, "OneOf can only be created with more than one alternative")
    require(alternatives.forall(!_.isInstanceOf[OneOf]), s"OneOf shouldn't be nested: ${alternatives.mkString(",")}")
  }
  object OneOf {
    def apply(fst: JsType, others: JsType*): OneOf =
      OneOf(fst +: others toSet)
  }
  final case class ObjectOf(fields: Map[String, JsType]) extends JsType
  object ObjectOf {
    def apply(fst: (String, JsType), others: (String, JsType)*): ObjectOf =
      ObjectOf((fst +: others).toMap)
  }

  sealed trait PrimitiveType extends JsType

  case object StringType extends PrimitiveType
  case object NumberType extends PrimitiveType
  case object BooleanType extends PrimitiveType

  case object NullType extends JsType

  // TODO: decide if that's really necessary or if it can be handled by OneOf(NullType, others) more uniformly
  final case class ValueOrNull(valueStructure: JsType) extends JsType

  case object Missing extends JsType

  /** Converts all constants to their abstract types */
  def widen(tpe: JsType): JsType = tpe match {
    case Constant(_, tpe)   => widen(tpe)
    case p: PrimitiveType   => p
    case NullType           => NullType
    case Missing            => Missing
    case ValueOrNull(v)     => ValueOrNull(widen(v))
    case ArrayOf(structure) => ArrayOf(widen(structure))
    case ObjectOf(fields)   => ObjectOf(fields.view.mapValues(widen).toMap)
    case OneOf(alternatives) =>
      val result = alternatives.map(widen)
      if (result.size == 1) result.head
      else OneOf(result)
  }

  def toJson(s: JsType): JsValue = s match {
    case ArrayOf(els) => JsArray(toJson(els))
    case ObjectOf(fields) =>
      val entries = fields.view.mapValues(toJson).toMap
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