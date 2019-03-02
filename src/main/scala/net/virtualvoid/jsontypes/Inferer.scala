package net.virtualvoid.jsontypes

import spray.json._
import JsType._

object Inferer {
  def infer(values: Seq[JsValue]): JsType = {
    require(values.nonEmpty, "Must give non-empty Seq to infer")
    values.map(infer).reduceLeftOption(unify).get
  }
  def infer(value: JsValue): JsType = value match {
    case _: JsString  => StringType
    case JsNull       => NullType
    case _: JsNumber  => NumberType
    case _: JsBoolean => BooleanType

    case array: JsArray =>
      if (array.elements.isEmpty) EmptyArray
      else ArrayOf(infer(array.elements))

    case obj: JsObject =>
      ObjectOf(obj.fields.mapValues(infer))
  }
  def unify(one: JsType, two: JsType): JsType = {
    def addToOneOf(existing: Alternatives, newEntry: JsType): Alternatives = {
      def tryOne(remaining: Seq[JsType], tried: Seq[JsType]): Alternatives =
        remaining match {
          case Nil =>
            existing + newEntry // cannot be unified into any of the existing alternatives
          case next +: rem =>
            unify(next, newEntry) match {
              case _: OneOf =>
                // didn't work, got alternative
                tryOne(rem, tried :+ next)
              case x =>
                // did work! use the new entry
                tried.toSet ++ rem.toSet + x
            }
        }

      tryOne(existing.toVector, Nil)
    }
    implicit val typeOrdering = Ordering.by[JsType, Int] {
      case NullType       => 0
      case _: ValueOrNull => 5
      case _: OneOf       => 10
      case _: ArrayOf     => 20
      case _: ObjectOf    => 30
      case StringType     => 40
      case NumberType     => 50
      case BooleanType    => 60
      case Missing        => 70
      case EmptyArray     => 80
    }
    import Ordering.Implicits._

    if (one == two) one
    else {
      // as unification is commutative, ensure we only have to look for one permutation
      val (fst, snd) =
        if (one < two) (one, two)
        else (two, one)

      (fst, snd) match {
        case (NullType, NullType) => NullType
        case (NullType, other: ValueOrNull) => other
        case (von @ ValueOrNull(v), v2) if unify(v, v2).isInstanceOf[PrimitiveType] => von
        case (NullType, other) => ValueOrNull(other)
        case (other: ArrayOf, EmptyArray) => other
        case (ArrayOf(s1), ArrayOf(s2)) => ArrayOf(unify(s1, s2))
        case (ObjectOf(fields1), ObjectOf(fields2)) =>
          val allFields = fields1.keySet ++ fields2.keySet

          val newStruct =
            allFields.map { f =>
              f -> unify(fields1.getOrElse(f, Missing), fields2.getOrElse(f, Missing))
            }.toMap
          ObjectOf(newStruct)

        case (OneOf(many), OneOf(others)) => OneOf(others.foldLeft(many)(addToOneOf))
        case (OneOf(many), x)             => OneOf(addToOneOf(many, x))
        case (a, b)                       => OneOf(Set(a, b))
      }
    }
  }
}
