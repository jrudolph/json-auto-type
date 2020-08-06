package net.virtualvoid.jsontypes
package web

import java.util.Random

import akka.http.scaladsl.Http
import akka.http.scaladsl.marshalling.{ Marshaller, ToEntityMarshaller }
import akka.http.scaladsl.model.{ HttpRequest, MediaTypes }
import akka.http.scaladsl.server.{ Directives, Route }
import akka.http.scaladsl.unmarshalling.Unmarshal
import spray.json._
import play.twirl.api.Html

import scala.concurrent.Future
import scala.util.Try

class Webservice(shutdownSignal: Future[Unit], autoreload: Boolean) extends Directives {
  implicit val twirlHtmlMarshaller: ToEntityMarshaller[Html] =
    Marshaller.StringMarshaller.wrap(MediaTypes.`text/html`)(_.toString)

  val random = new Random()

  lazy val route: Route =
    concat(
      (pathSingleSlash & get) { complete(html.page(html.form())) },
      (path("analyze") & post) { formField("json")(jsonStr => analyze(jsonStr.parseJson)) },
      (path("analyzeURI") & get) {
        parameter("uri") { uri =>
          extractActorSystem { implicit system =>
            extractExecutionContext { implicit ec =>
              import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
              val responseF =
                Http().singleRequest(HttpRequest(uri = uri.trim))
                  .flatMap(res => Unmarshal(res).to[JsValue])
              onSuccess(responseF)(analyze)
            }
          }
        }
      },
      // Scala-JS puts them in the root of the resource directory per default,
      // so that's where we pick them up
      path("frontend-fastopt.js")(getFromResource("frontend-fastopt.js")),
      path("frontend-fastopt.js.map")(getFromResource("frontend-fastopt.js.map")),
      if (autoreload) path("ws-watchdog") { AutoReloaderRoute(shutdownSignal) } else reject,
      getFromResourceDirectory("web"),
    )

  def analyze(json: JsValue): Route = {
    val inferer = new Inferer()
    val tpe = Try(inferer.inferAndUnify(json :: Nil))
    val binding = tpe.map(SprayJsonCodeGen.bindingFor).getOrElse("<error>")
    val structure = tpe.map(JsType.toJson(_).prettyPrint).getOrElse("<error>")
    complete(html.page(html.result(json.prettyPrint, structure, binding)))
  }
}
