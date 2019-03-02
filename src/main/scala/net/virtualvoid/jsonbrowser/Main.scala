package net.virtualvoid.jsonbrowser

import spray.json._

import scala.io.Source

object Main extends App {
  val data = Source.fromFile("test.json")
  val jsonEntries = data.getLines.map(_.parseJson)
  val asArray = JsArray(jsonEntries.toVector)

  type Alternatives = Set[JsonStructure]

  sealed trait JsonStructure
  final case class ArrayOf(structure: JsonStructure) extends JsonStructure
  final case class OneOf(alternatives: Alternatives) extends JsonStructure
  final case class ObjectOf(fields: Map[String, JsonStructure]) extends JsonStructure

  case class ValueOrNull(valueStructure: JsonStructure) extends JsonStructure

  sealed trait PrimitiveStructure extends JsonStructure

  case class JsStringStructure(value: String) extends PrimitiveStructure
  case object JsNumberStructure extends PrimitiveStructure
  case object JsBooleanStructure extends PrimitiveStructure

  case object JsNullStructure extends JsonStructure

  case object Missing extends JsonStructure
  case object EmptyArray extends JsonStructure

  def infer(value: JsValue): JsonStructure = value match {
    case JsString(value) => JsStringStructure(value)
    case JsNull          => JsNullStructure
    case _: JsNumber     => JsNumberStructure
    case _: JsBoolean    => JsBooleanStructure

    case array: JsArray =>
      array.elements.map(infer).reduceLeftOption(unify).map(ArrayOf).getOrElse(EmptyArray)
    case obj: JsObject =>
      ObjectOf(obj.fields.mapValues(infer))
  }
  def unify(one: JsonStructure, two: JsonStructure): JsonStructure = {
    def addToOneOf(existing: Alternatives, newEntry: JsonStructure): Alternatives = {
      def tryOne(remaining: Seq[JsonStructure], tried: Seq[JsonStructure]): Alternatives =
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
    def ordering(structure: JsonStructure): Int = structure match {
      case JsNullStructure      => 0
      case _: ValueOrNull       => 5
      case _: OneOf             => 10
      case _: ArrayOf           => 20
      case _: ObjectOf          => 30
      case _: JsStringStructure => 40
      case JsNumberStructure    => 50
      case JsBooleanStructure   => 60
      case Missing              => 70
      case EmptyArray           => 80
    }

    if (one == two) one
    else {
      // as unification is commutative, ensure we only have to look for one permutation
      val (fst, snd) =
        if (ordering(one) < ordering(two)) (one, two)
        else (two, one)

      (fst, snd) match {
        case (JsNullStructure, JsNullStructure) => JsNullStructure
        case (JsNullStructure, other: ValueOrNull) => other
        case (von @ ValueOrNull(v), v2) if unify(v, v2).isInstanceOf[PrimitiveStructure] => von
        case (JsNullStructure, other) => ValueOrNull(other)
        case (_: JsStringStructure, _: JsStringStructure) => JsStringStructure("<several>")
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

  def toJson(s: JsonStructure): JsValue = s match {
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

  println(toJson(infer(asArray)).prettyPrint)
}
