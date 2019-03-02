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

  def infer(value: JsValue): JsType = value match {
    case JsString(value) => StringType(value)
    case JsNull          => NullType
    case _: JsNumber     => NumberType
    case _: JsBoolean    => BooleanType

    case array: JsArray =>
      array.elements.map(infer).reduceLeftOption(unify).map(ArrayOf).getOrElse(EmptyArray)
    case obj: JsObject =>
      ObjectOf(obj.fields.mapValues(infer))
  }
  def unify(one: JsType, two: JsType): JsType = {
    def addToOneOf(existing: Alternatives, newEntry: JsType): Alternatives = {
      def tryOne(remaining: Seq[JsType], tried: Seq[JsType]): Alternatives =
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
    def ordering(structure: JsType): Int = structure match {
      case JsType.NullType      => 0
      case _: ValueOrNull       => 5
      case _: OneOf             => 10
      case _: ArrayOf           => 20
      case _: ObjectOf          => 30
      case _: JsType.StringType => 40
      case JsType.NumberType    => 50
      case JsType.BooleanType   => 60
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
        case (JsType.NullType, JsType.NullType) => JsType.NullType
        case (JsType.NullType, other: ValueOrNull) => other
        case (von @ ValueOrNull(v), v2) if unify(v, v2).isInstanceOf[PrimitiveType] => von
        case (JsType.NullType, other) => ValueOrNull(other)
        case (_: JsType.StringType, _: JsType.StringType) => StringType("<several>")
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

  def toJson(s: JsType): JsValue = s match {
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

  val structure = infer(json)
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
