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
}
