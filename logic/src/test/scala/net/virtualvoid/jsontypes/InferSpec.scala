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

      "OneOf" - {
        "for two different types" in {
          Seq(
            "1234",
            "true"
          ) must inferTo(OneOf(NumberType, BooleanType))
        }
        "for three different types" in {
          Seq(
            "1234",
            "true",
            "\"test\""
          ) must inferTo(OneOf(NumberType, StringType, BooleanType))
        }
        "pull out ValueOrNull from OneOf" in {
          Seq(
            "1234",
            "null",
            "true"
          ) must inferTo(ValueOrNull(OneOf(NumberType, BooleanType)))
          Seq(
            "1234",
            "true",
            "null",
            "\"test\""
          ) must inferTo(ValueOrNull(OneOf(NumberType, BooleanType, StringType)))
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
          ObjectOf("name" -> StringType, "age" -> NumberType)
        }
      }
      "unify same structured" in {
        Seq(
          """{ "name": "Paula", "age": 46}""",
          """{ "name": "Gustav", "age": 52}"""
        ) must inferTo {
            ObjectOf("name" -> StringType, "age" -> NumberType)
          }
      }
      "unify element-wise" in {
        Seq(
          """{ "name": "Paula", "age": 46}""",
          """{ "name": null, "age": "52"}"""
        ) must inferTo {
            ObjectOf("name" -> ValueOrNull(StringType), "age" -> OneOf(NumberType, StringType))
          }
      }
      "note missing elements" in {
        Seq(
          """{ "name": "Paula", "age": 46}""",
          """{ "name": "Gustav", "age": 52, "house_color": "glitter"}"""
        ) must inferTo {
            ObjectOf("name" -> StringType, "age" -> NumberType, "house_color" -> OneOf(Missing, StringType))
          }
      }
    }

    "detect constant values if enabled" - {
      "field of object" in {
        Seq(
          """{"name": "Haensel", "age": 7}""",
          """{"name": "Gretel", "age": 7}"""
        ) must inferTo(
            ObjectOf("name" -> StringType, "age" -> Constant(JsNumber(7), NumberType)), InferSettings.default
          )
      }
    }

    "discriminate object types" - {
      "if ratio of shared field names is too low" in {
        Seq(
          """{"name": "Haensel", "age": 7}""",
          """{"first_name": "Haensel", "last_name": "Meier"}"""
        ) must inferTo(
            OneOf(
              ObjectOf("first_name" -> StringType, "last_name" -> StringType),
              ObjectOf("name" -> StringType, "age" -> NumberType)
            )
          )
      }
      "not if ratio of shared field names is high enough" in {
        Seq(
          """{"first_name": "Haensel", "last_name": "Meier", "age": 7}""",
          """{"first_name": "Gretel", "last_name": "Mayer"}"""
        ) must inferTo(
            ObjectOf("first_name" -> StringType, "last_name" -> StringType, "age" -> OneOf(NumberType, Missing))
          )
      }

      "by discriminator field" in {
        Seq(
          """{"type": "cat", "name": "Gerlinda", "age": 7, "meow_noise":"meeeeeeeooooww"}""",
          """{"type": "cat", "name": "GigA", "age": 4, "meow_noise":"aaaaaaarrrrrrr"}""",
          """{"type": "dog", "name": "Nana", "age": 12, "hair_form":"curly", "favorite_tree":"oak"}""",
          """{"type": "dog", "name": "Gonzo", "age": 4, "hair_form":"straight"}"""
        ) must inferTo(
            OneOf(
              ObjectOf("type" -> Constant(JsString("cat"), StringType), "name" -> StringType, "age" -> NumberType, "meow_noise" -> StringType),
              ObjectOf("type" -> Constant(JsString("dog"), StringType), "name" -> StringType, "favorite_tree" -> OneOf(StringType, Missing), "age" -> NumberType, "hair_form" -> StringType)
            ),
            InferSettings.default)
      }
    }
  }

  def inferTo(expected: JsType, settings: InferSettings = InferSettings.noConstants): Matcher[Seq[String]] =
    Matcher { jsonStrings: Seq[String] =>
      val json = jsonStrings.map(_.parseJson)
      val tpe = new Inferer(settings).inferAndUnify(json)

      MatchResult(
        tpe == expected,
        s"${jsonStrings.mkString("'", ", ", "'")} should have type '$expected' but had '$tpe'",
        s"${jsonStrings.mkString("'", ", ", "'")} had expected type '$expected'"
      )
    }

  def haveType(expected: JsType, settings: InferSettings = InferSettings.noConstants): Matcher[String] =
    Matcher { jsonString: String =>
      val json = jsonString.parseJson
      val tpe = new Inferer(settings).infer(json)

      MatchResult(
        tpe == expected,
        s"'$jsonString' should have type '$expected' but had '$tpe'",
        s"'$jsonString' had expected type '$expected'"
      )
    }
}
