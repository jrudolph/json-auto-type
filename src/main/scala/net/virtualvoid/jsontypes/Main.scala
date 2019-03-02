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

  case class Metadata(typeName: String, definition: String = "", innerDefs: Seq[Metadata] = Nil)
  val ReservedKeywords = Set("type")
  val SpecialChars = "[^\\w]+".r
  def isClean(name: String): Boolean =
    !(ReservedKeywords.contains(name) || SpecialChars.findFirstMatchIn(name).isDefined)
  def clean(name: String): String =
    name.replaceAll(SpecialChars.regex, "_")

  def camelCased(name: String): String =
    if (Character.isLowerCase(name(0))) name(0).toUpper +: name.tail mkString
    else name
  def toCaseClasses(s: JsType, namePrefix: String): Metadata =
    s match {
      case JsType.BooleanType   => Metadata("Boolean")
      case JsType.NumberType    => Metadata("BigDecimal")
      case JsType.NullType      => Metadata("Option[String]")
      case _: JsType.StringType => Metadata("String")
      case ArrayOf(struct) =>
        val meta = toCaseClasses(struct, namePrefix + "ArrayElement")
        Metadata(s"Seq[${meta.typeName}]", innerDefs = meta :: Nil)
      case ObjectOf(fields) =>
        val name = camelCased(namePrefix)
        val (fieldDefs: Seq[String], metas: Seq[Metadata]) =
          fields
            .filterNot(_._1 contains ":")
            //.take(22) // what can we do?
            .toSeq
            .sortBy(_._1)
            .map {
              case (name, struct) =>
                val finalName = if (isClean(name)) name else s"`$name`"
                val m = toCaseClasses(struct, clean(name))
                (s"$finalName: ${m.typeName}", m)
            }.unzip
        Metadata(
          name,
          s"case class $name(\n${fieldDefs.mkString(",\n")})\nimplicit val ${name}Format = jsonFormat${fieldDefs.size}($name.apply _)",
          innerDefs = metas)

      case OneOf(alternatives) =>
        if (alternatives.size == 2 && alternatives.contains(Missing)) {
          val s = (alternatives - Missing).head
          val meta = toCaseClasses(s, namePrefix + "OptionElement")
          Metadata(s"Option[${meta.typeName}]", innerDefs = meta :: Nil)
        } else
          throw new IllegalArgumentException("Only optional alternatives supported, right now")

      case ValueOrNull(valueStructure) =>
        val meta = toCaseClasses(valueStructure, namePrefix + "OptionElement")
        val name = camelCased(namePrefix)
        Metadata(s"Option[${meta.typeName}]", innerDefs = meta :: Nil)
      case EmptyArray => Metadata("Seq[String]") // if we could we would just omit it
    }

  def printMetadata(m: Metadata): String = {
    def collectDefs(m: Metadata): Seq[String] = {
      val inners = m.innerDefs.flatMap(collectDefs)
      if (m.definition.nonEmpty)
        inners :+ m.definition
      else
        inners
    }

    collectDefs(m).mkString("\n")
  }

  val structure = Inferer.infer(json)
  println(toJson(structure).prettyPrint)
  println(printMetadata(toCaseClasses(structure, "Root")))

  object GitHubTest {
    import DefaultJsonProtocol._

    case class CasesArrayElement(skipped: Boolean, stdout: String, duration: BigDecimal, skippedMessage: Option[String], status: String, className: String, errorStackTrace: Option[String], stderr: String, testActions: Seq[String], errorDetails: Option[String], name: String, failedSince: BigDecimal, age: BigDecimal)
    implicit val CasesArrayElementFormat = jsonFormat13(CasesArrayElement.apply _)
    case class SuitesArrayElement(stdout: String, duration: BigDecimal, id: Option[String], stderr: String, name: String, nodeId: Option[String], cases: Seq[CasesArrayElement], timestamp: String, enclosingBlocks: Seq[String], enclosingBlockNames: Seq[String])
    implicit val SuitesArrayElementFormat = jsonFormat10(SuitesArrayElement.apply _)
    case class Root(passCount: BigDecimal, _class: String, suites: Seq[SuitesArrayElement], skipCount: BigDecimal, duration: BigDecimal, empty: Boolean, failCount: BigDecimal, testActions: Seq[String])
    implicit val RootFormat = jsonFormat8(Root.apply _)

  }
  {
    import DefaultJsonProtocol._
    import GitHubTest._
    val res =
      try json.convertTo[Root]
      catch {
        case e @ DeserializationException(msg, cause, fieldNames) =>
          println(s"Error for $fieldNames")
          e.printStackTrace()
      }
    println(res)
  }
  /*object Test {
    import DefaultJsonProtocol._

    case class Logging(version: String)
    implicit val LoggingFormat = jsonFormat1(Logging.apply _)
    case class SourceOptionElement(line: String, file: String, enclosing: String)
    implicit val SourceOptionElementFormat = jsonFormat3(SourceOptionElement.apply _)
    case class OrderOptionElement(merchantId: String, PMLN: String, orderToken: String, orderId: String, orderState: String)
    implicit val OrderOptionElementFormat = jsonFormat5(OrderOptionElement.apply _)
    case class Labels(`pod-template-hash`: String, domain: String, `repo-branch`: String, service: String, namespace: String, `image-version`: String, `manifest-version`: String)
    implicit val LabelsFormat = jsonFormat7(Labels.apply _)
    case class Kubernetes(container_name: String, master_url: String, labels: Labels, pod_name: String, namespace_name: String, host: String, pod_id: String, namespace_id: String)
    implicit val KubernetesFormat = jsonFormat8(Kubernetes.apply _)
    case class Tags(`k8s.io/cluster-autoscaler/disabled`: String, NodeType: String, Monitor: String, KubernetesCluster: String, tectonicClusterID: String, Environment: String, spotInstances: String, Name: String, `kubernetes.io/cluster/gamma-tectonic78b1efab`: String)
    implicit val TagsFormat = jsonFormat9(Tags.apply _)
    case class Ec2(instance_id: String, hostname: String, security_groups: String, tags: Tags)
    implicit val Ec2Format = jsonFormat4(Ec2.apply _)
    case class Docker(container_id: String)
    implicit val DockerFormat = jsonFormat1(Docker.apply _)
    case class Event(httpRequestHeaders: Option[String], logging: Logging, httpResponseHeaders: Option[String], httpRequestPath: Option[String], httpResponseStatusCode: Option[String], kafkaOffset: Option[String], httpRequestMethod: Option[String], kafkaMessageClass: Option[String], source: Option[SourceOptionElement], kafkaPartition: Option[String], `@timestamp`: String, order: Option[OrderOptionElement], kubernetes: Kubernetes, loggerName: String, merchantId: Option[String], ec2: Ec2, docker: Docker, timeMillis: String, kafkaTopic: Option[String], thread: String, kafkaMessageBody: Option[String], httpResponseTime: Option[String])
    implicit val EventFormat = jsonFormat22(Event.apply _)
    case class ArrayElement(event: Event, source: String)
    implicit val ArrayElementFormat = jsonFormat2(ArrayElement.apply _)

  }
  {
    import DefaultJsonProtocol._
    import Test._
    val res = asArray.convertTo[Seq[ArrayElement]]
    println(res)
  }*/
}
