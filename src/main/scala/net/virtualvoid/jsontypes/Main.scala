package net.virtualvoid.jsontypes

import spray.json._

import scala.io.Source

object Main extends App {
  val data = Source.fromFile("jenkins-project-with-parameters.json")
  /*val jsonEntries =
    data.getLines
      .take(50)
      .map(_.parseJson)
  val asArray = JsArray(jsonEntries.toVector)*/
  val json = data.mkString.parseJson
  import JsType._

  val tpe = Inferer.infer(json)
  println(toJson(tpe).prettyPrint)
  println(SprayJsonCodeGen.bindingFor(tpe))
}
