package net.virtualvoid.jsontypes

import spray.json._
import JsType._

import scala.annotation.tailrec

object Inferer {
  def inferAndUnify(values: Seq[JsValue]): JsType =
    values.map(infer).reduceLeftOption(unify).getOrElse(Missing)

  def infer(value: JsValue): JsType = value match {
    case _: JsString    => StringType
    case JsNull         => NullType
    case _: JsNumber    => NumberType
    case _: JsBoolean   => BooleanType

    case array: JsArray => ArrayOf(inferAndUnify(array.elements))
    case obj: JsObject  => ObjectOf(obj.fields.mapValues(infer))
  }
  def unify(one: JsType, two: JsType): JsType = {
    def addToOneOf(existing: Alternatives, newEntry: JsType): Alternatives = {
      @tailrec def tryOne(remaining: Seq[JsType], tried: Seq[JsType]): Alternatives =
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

    // arbitrary order to be able to compare two elements, so that we only need to consider one of the two possible
    // permutations (unification is commutative)
    implicit val typeOrdering = Ordering.by[JsType, Int] {
      case NullType         => 0
      case _: ValueOrNull   => 5
      case _: OneOf         => 10
      case ArrayOf(Missing) => 19
      case _: ArrayOf       => 20
      case _: ObjectOf      => 30
      case StringType       => 40
      case NumberType       => 50
      case BooleanType      => 60
      case Missing          => 70
    }
    import Ordering.Implicits._

    if (one == two) one
    else {
      // as unification is commutative, ensure we only have to look for one permutation
      val (fst, snd) =
        if (one < two) (one, two)
        else (two, one)

      (fst, snd) match {
        case (NullType, NullType)                  => NullType
        case (NullType, other: ValueOrNull)        => other
        case (NullType, other)                     => ValueOrNull(other)

        case (von @ ValueOrNull(v), v2) if v == v2 => von

        case (ArrayOf(Missing), ArrayOf(other))    => ArrayOf(other)
        case (ArrayOf(s1), ArrayOf(s2))            => ArrayOf(unify(s1, s2))

        case (ObjectOf(fields1), ObjectOf(fields2)) =>
          val allFields = fields1.keySet ++ fields2.keySet

          // Here, I made a particular choice in the algorithm: every field is unified independently of every other.
          // If there's an array of objects that is homogeneous, i.e. every element is representing a similar data item (e.g. a user)
          // but every element might have slight differences (some fields might be optional or heterogeneous), then this
          // algorithm will lead to good results.
          //
          // For heterogeneous arrays, however, it leads to suboptimal results for two elements that are each of a
          // different kind cannot be unified gracefully. Instead, the result will be a big object type where e.g. each
          // field is optional or a OneOf. For those cases, better heuristics need to be developed. For example, if two objects
          // share too few common fields, one could treat them as two different objects types in the first place. Another
          // heuristic could be to detect discriminator fields with common names like '_class', or '_type` and use those
          // as an indication or trigger to prevent unification of all the fields.
          val newStruct =
            allFields.map { f =>
              f -> unify(fields1.getOrElse(f, Missing), fields2.getOrElse(f, Missing))
            }.toMap
          ObjectOf(newStruct)

        case (OneOf(many), OneOf(others)) => OneOf(others.foldLeft(many)(addToOneOf))
        case (OneOf(many), x)             => OneOf(addToOneOf(many, x))

        // case that both are the same was handled above, so remaining case is two distinct
        // types that cannot be further unified
        case (a, b)                       => OneOf(Set(a, b))
      }
    }
  }
}
