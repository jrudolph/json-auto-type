package net.virtualvoid.jsontypes

import scala.io.Source
import spray.json._

object Main extends App {
  val data = Source.fromResource("github-user-events.json")
  /*val jsonEntries =
    data.getLines
      .take(50)
      .map(_.parseJson)
  val asArray = JsArray(jsonEntries.toVector)*/
  val json = data.mkString.parseJson
  import JsType._

  val tpe = new Inferer().infer(json)
  println(toJson(tpe).prettyPrint)
  //println(SprayJsonCodeGen.bindingFor(tpe))
}
