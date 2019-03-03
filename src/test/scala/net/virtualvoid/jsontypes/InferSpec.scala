package net.virtualvoid.jsontypes

import org.scalatest.FreeSpec
import org.scalatest.MustMatchers
import org.scalatest.matchers.MatchResult
import org.scalatest.matchers.Matcher
import spray.json._

import JsType._

class InferSpec extends FreeSpec with MustMatchers {
  "Inferer should infer" - {
    "primitive types" - {
      "null" in { "null" must haveType(NullType) }
      "string" in { "\"abc\"" must haveType(StringType) }
      "number" in { "12345" must haveType(NumberType) }
      "boolean" in { "true" must haveType(BooleanType) }
    }
    "other simple types" - {
      "empty array" in { "[]" must haveType(ArrayOf(Missing)) }

      "ValueOrNull" - {
        "simple case" in {
          Seq("null", "1234") must inferTo(ValueOrNull(NumberType))
        }
        "multiple nulls" in {
          Seq("null", "1234", "null") must inferTo(ValueOrNull(NumberType))
        }
        "multiple values" in {
          Seq("42", "null", "1234") must inferTo(ValueOrNull(NumberType))
        }
      }
    }
    "from array elements" - {
      "string array" in { "[\"a\", \"b\"]" must haveType(ArrayOf(StringType)) }

      "unify types of two arrays" - {
        "when both arrays have elements of the same type" in {
          Seq(
            "[true, false]",
            "[false, true]"
          ) must inferTo(ArrayOf(BooleanType))
        }
        "when both arrays have elements, but one only null" in {
          Seq(
            "[1234]",
            "[null]"
          ) must inferTo(ArrayOf(ValueOrNull(NumberType)))
        }
        "when one array is empty" in {
          Seq(
            "[1234]",
            "[]"
          ) must inferTo(ArrayOf(NumberType))
        }
      }
    }
    "from objects" - {
      "simple" in {
        """{ "name": "Paula", "age": 46}""" must haveType {
          ObjectOf(Map("name" -> StringType, "age" -> NumberType))
        }
      }
      "unify same structured" in {
        Seq(
          """{ "name": "Paula", "age": 46}""",
          """{ "name": "Gustav", "age": 52}"""
        ) must inferTo {
            ObjectOf(Map("name" -> StringType, "age" -> NumberType))
          }
      }
      "unify element-wise" in {
        Seq(
          """{ "name": "Paula", "age": 46}""",
          """{ "name": null, "age": "52"}"""
        ) must inferTo {
            ObjectOf(Map("name" -> ValueOrNull(StringType), "age" -> OneOf(NumberType, StringType)))
          }
      }
      "note missing elements" in {
        Seq(
          """{ "name": "Paula", "age": 46}""",
          """{ "name": "Gustav", "age": 52, "house_color": "glitter"}"""
        ) must inferTo {
            ObjectOf(Map("name" -> StringType, "age" -> NumberType, "house_color" -> OneOf(Missing, StringType)))
          }
      }

    }
  }

  def inferTo(expected: JsType): Matcher[Seq[String]] =
    Matcher { jsonStrings: Seq[String] =>
      val json = jsonStrings.map(_.parseJson)
      val tpe = Inferer.inferAndUnify(json)

      MatchResult(
        tpe == expected,
        s"${jsonStrings.mkString("'", ", ", "'")} should have type '$expected' but had '$tpe'",
        s"${jsonStrings.mkString("'", ", ", "'")} had expected type '$expected'"
      )
    }

  def haveType(expected: JsType): Matcher[String] =
    Matcher { jsonString: String =>
      val json = jsonString.parseJson
      val tpe = Inferer.infer(json)

      MatchResult(
        tpe == expected,
        s"'$jsonString' should have type '$expected' but had '$tpe'",
        s"'$jsonString' had expected type '$expected'"
      )
    }
}
