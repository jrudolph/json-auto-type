package net.virtualvoid.jsontypes

sealed trait JsType
object JsType {
  type Alternatives = Set[JsType]

  final case class ArrayOf(structure: JsType) extends JsType
  final case class OneOf(alternatives: Alternatives) extends JsType
  final case class ObjectOf(fields: Map[String, JsType]) extends JsType

  sealed trait PrimitiveType extends JsType

  case class StringType(value: String) extends PrimitiveType
  case object NumberType extends PrimitiveType
  case object BooleanType extends PrimitiveType
  case object NullType extends JsType

  case class ValueOrNull(valueStructure: JsType) extends JsType

  case object Missing extends JsType
  case object EmptyArray extends JsType
}