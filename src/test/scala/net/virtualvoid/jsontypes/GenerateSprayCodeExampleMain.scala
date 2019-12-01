package net.virtualvoid.jsontypes

import scala.io.Source
import spray.json._

object GenerateSprayCodeExampleMain extends App {
  val inputFiles: Seq[String] = Seq(
    "/test.json"
  )

  val datas = JsArray(
    inputFiles.map(f => Source.fromFile(f).mkString.parseJson).toVector
  )

  import JsType._

  val ArrayOf(tpe) = new Inferer().infer(datas)
  println(toJson(tpe).prettyPrint)

  val tpesToShow =
    tpe match {
      case ArrayOf(OneOf(several)) => several.toList
      case _ =>
        tpe :: Nil

    }

  tpesToShow.foreach { tpe =>
    println(SprayJsonCodeGen.bindingFor(tpe))
  }
}
