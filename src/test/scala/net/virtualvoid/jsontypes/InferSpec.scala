package net.virtualvoid.jsontypes

import org.scalatest.FreeSpec
import org.scalatest.MustMatchers
import org.scalatest.matchers.MatchResult
import org.scalatest.matchers.Matcher
import spray.json._

class InferSpec extends FreeSpec with MustMatchers {
  "Inferer should infer" - {
    "primitive types" - {
      "null" in { "null" must haveType(JsType.NullType) }
      "string" in { "\"abc\"" must haveType(JsType.StringType) }
      "number" in { "12345" must haveType(JsType.NumberType) }
      "boolean" in { "true" must haveType(JsType.BooleanType) }
    }
    "other simple types" - {
      "empty array" in { "[]" must haveType(JsType.EmptyArray) }
    }
    "from array elements" - {
      "string array" in { "[\"a\", \"b\"]" must haveType(JsType.ArrayOf(JsType.StringType)) }
    }
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
