package net.virtualvoid.jsontypes
package web

import java.util.Random

import akka.http.scaladsl.marshalling.{ Marshaller, ToEntityMarshaller }
import akka.http.scaladsl.model.MediaTypes
import akka.http.scaladsl.server.{ Directives, Route }
import spray.json._
import play.twirl.api.Html

import scala.concurrent.Future

class Webservice(shutdownSignal: Future[Unit], autoreload: Boolean) extends Directives {
  implicit val twirlHtmlMarshaller: ToEntityMarshaller[Html] =
    Marshaller.StringMarshaller.wrap(MediaTypes.`text/html`)(_.toString)

  val random = new Random()

  lazy val route: Route =
    concat(
      get {
        concat(
          pathSingleSlash {
            complete(html.page(html.form()))
          },
          // Scala-JS puts them in the root of the resource directory per default,
          // so that's where we pick them up
          path("frontend-fastopt.js")(getFromResource("frontend-fastopt.js")),
          path("frontend-fastopt.js.map")(getFromResource("frontend-fastopt.js.map")),
          if (autoreload) path("ws-watchdog") { AutoReloaderRoute(shutdownSignal) } else reject,
        )
      },
      post {
        concat(
          path("analyze") {
            formField("json") { jsonStr =>
              val json = jsonStr.parseJson
              val inferer = new Inferer()
              val tpe = inferer.inferAndUnify(json :: Nil)
              val res = SprayJsonCodeGen.bindingFor(tpe)
              complete(html.page(html.result(json.prettyPrint, JsType.toJson(tpe).prettyPrint, res)))
            }
          },
        )
      },
      getFromResourceDirectory("web"),
    )
}
