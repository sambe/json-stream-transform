package org.github.sambe.jsonstreamtransform

import java.io.{IOException, OutputStream, InputStream}
import com.fasterxml.jackson.core._
import com.fasterxml.jackson.core.util.DefaultPrettyPrinter
import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}
import com.fasterxml.jackson.databind.node.{JsonNodeFactory, ArrayNode, ObjectNode}
import scala.collection.JavaConverters.{iterableAsScalaIterableConverter, asScalaIteratorConverter, asJavaIterableConverter}
import Util._

class Transform(matcher: Matcher) {

  val mapper = new ObjectMapper()
  val jsonFactory = mapper.getFactory
  val nodeFactory = new JsonNodeFactory(false)

  def transform(is: InputStream, os: OutputStream) = autoCloseStreams(is, os) {
    val parser = jsonFactory.createParser(is)
    val generator = jsonFactory.createGenerator(os)
    generator.setPrettyPrinter(new DefaultPrettyPrinter())

    if (parser.nextToken() != JsonToken.START_OBJECT) {
      sys.error("Expected JSON document to start with {")
    }
    transformObject(Some(matcher), true)(parser, generator)
    generator.flush // important, without this not everything will be written
  }

  private def transformObject(matcher: Option[Matcher], copy: Boolean)(implicit p: JsonParser, g: JsonGenerator) {
    matcher match {
        // if matcher has a condition, the tree structure below this must be parsed into an object structure for further processing (e.g. evaluating the condition)
      case Some(Matcher(_, _, _, Some(condition))) if copy => transformObjectAsTree(matcher)
      case _ => transformObjectStream(matcher, copy)
    }
  }

  private def transformObjectStream(matcher: Option[Matcher], copy: Boolean)(implicit p: JsonParser, g: JsonGenerator) {
    val removeLevelInner = matchTransformer(matcher){case RemoveLevel(inner) => inner}
    val copyThis = copy && !removeLevelInner.isDefined
    if (copyThis)
      g.writeStartObject()
    while(p.nextToken != JsonToken.END_OBJECT) {
      val fieldName = p.getCurrentName
      val fieldCopy = copyThis && matchTransformer(matcher){case RemoveAttribute(`fieldName`) => false}.getOrElse(true)
      if (fieldCopy) {
        val fieldCopyName = matchTransformer(matcher){case RenameAttribute(`fieldName`,newName) => newName}.getOrElse(fieldName)
        g.writeFieldName(fieldCopyName)
      }
      val fieldMatcher = subMatcher(matcher, fieldName)
      val replaceValue = matchTransformer(fieldMatcher){case ReplaceValue(value) => value}
      replaceValue.foreach(streamWriteValue(_, fieldName))
      val valueCopy = (fieldCopy || removeLevelInner.exists(_ == fieldName)) && !replaceValue.isDefined
      transformValue(fieldMatcher, valueCopy)
    }
    val inserts = matchTransformers(matcher){case ia: InsertAttribute => ia }
    inserts.foreach { ia =>
      g.writeFieldName(ia.name)
      streamWriteValue(ia.value, ia.name)
    }
    if (copyThis)
      g.writeEndObject()
  }

  private def streamWriteValue(value: Any, name: String)(implicit g: JsonGenerator) {
    value match {
      case s: String => g.writeString(s)
      case i: Int => g.writeNumber(i)
      case l: Long => g.writeNumber(l)
      case bi: BigInt => g.writeNumber(bi.bigInteger)
      case bi: java.math.BigInteger => g.writeNumber(bi)
      case bd: BigDecimal => g.writeNumber(bd.bigDecimal)
      case bd: java.math.BigDecimal => g.writeNumber(bd)
      case d: Double => g.writeNumber(d)
      case f: Float => g.writeNumber(f)
      case b: Boolean => g.writeBoolean(b)
      case null => g.writeNull()
      case s: Seq[_] =>
        g.writeStartArray()
        s.foreach(streamWriteValue(_, name))
        g.writeEndArray()
      case m: Map[_,_] =>
        g.writeStartObject()
        m.foreach { case (k,v) =>
          val fieldName = k.toString
          g.writeFieldName(fieldName)
          streamWriteValue(v, fieldName)
        }
        g.writeEndObject()
      case tn: TreeNode => g.writeTree(tn)
      case other => sys.error("value type of attribute %s not supported: %s" format (name, other.getClass.toString))
    }
  }

  private def createJsonNode(value: Any): JsonNode = {
    value match {
      case s: String => nodeFactory.textNode(s)
      case i: Int => nodeFactory.numberNode(i)
      case l: Long => nodeFactory.numberNode(l)
      case bi: BigInt => nodeFactory.numberNode(bi.bigInteger)
      case bi: java.math.BigInteger => nodeFactory.numberNode(bi)
      case bd: BigDecimal => nodeFactory.numberNode(bd.bigDecimal)
      case bd: java.math.BigDecimal => nodeFactory.numberNode(bd)
      case d: Double => nodeFactory.numberNode(d)
      case f: Float => nodeFactory.numberNode(f)
      case b: Boolean => nodeFactory.booleanNode(b)
      case null => nodeFactory.nullNode()
      case jn: JsonNode => jn
      case s: Seq[_] =>
        val an = nodeFactory.arrayNode()
        s.foreach { e =>
          an.add(createJsonNode(e))
        }
        an
      case m: Map[_,_] =>
        val on = nodeFactory.objectNode()
        m.foreach { case (k,v) =>
          on.set(k.toString, createJsonNode(v))
        }
        on
      case other => sys.error("value type not supported: %s" format other.getClass.toString)
    }
  }

  private def transformObjectAsTree(matcher: Option[Matcher])(implicit p: JsonParser, g: JsonGenerator) {
    val tree = mapper.readTree[JsonNode](p)
    matcher.foreach(transformObjectTree(_)(tree))
    g.writeTree(tree)
  }

  private def transformObjectTree(matcher: Matcher)(tree: JsonNode): JsonNode = {
    // only do something if the condition is true (if there is one on this level)
    if (matcher.condition.map(_(tree)).getOrElse(true)) {
      val replaceValue = matchTransformer(Some(matcher)){case ReplaceValue(value) => value}
      replaceValue.map { v =>
        createJsonNode(v)
      } getOrElse {
        // 1) recursive matching
        matcher.subMatchers.map { m =>
          tree match {
            case an: ArrayNode =>
              if (m.pattern == "*") {
                an.asScala.zipWithIndex.foreach { case (node, i) =>
                  val updated = transformObjectTree(m)(node)
                  if (updated != node) an.set(i, updated)
                }
              } else {
                val index: Int = safeToInt(m.pattern).getOrElse(sys.error("matcher of array is neither '*' nor a valid number: " + m.pattern))
                transformArrayNodeElement(m, index)(an)
              }
            case on: ObjectNode =>
              if (m.pattern == "*") {
                on.fieldNames().asScala.foreach { fieldName =>
                  transformObjectNodeField(m, fieldName)(on)
                }
              } else {
                transformObjectNodeField(m, m.pattern)(on)
              }
            case _ =>
              // no match
              None
          }
        }
        // 2) applying transformers at this level
        matcher.transformers.foreach {
          case ReplaceValue(value) =>
            sys.error("should never get to this point, because replacing earlier")
          case RenameAttribute(oldName, newName) =>
            onlyIfObjectNode(tree) { on =>
              val value = on.remove(oldName)
              if (value != null)
                on.set(newName, value)
            }
          case RemoveAttribute(name) =>
            onlyIfObjectNode(tree) { on =>
              on.remove(name)
            }
          case InsertAttribute(name, value) =>
            onlyIfObjectNode(tree) { on =>
              val jsonValue = createJsonNode(value)
              on.set(name, jsonValue)
            }
          case RemoveLevel(inner) =>
            onlyIfObjectNode(tree) { on =>
              val innerValue = on.get(inner)
              onlyIfObjectNode(innerValue) { innerNode =>
                innerNode.fieldNames().asScala.foreach { fieldName =>
                  on.set(fieldName, innerNode.get(fieldName))
                }
              }
              on.remove(inner)
            }
        }
        tree
      }
    } else tree
  }

  private def transformArrayNodeElement(m: Matcher, index: Int)(an: ArrayNode): ArrayNode = {
    if (index >= 1 || index <= an.size()) {

      val v = an.get(index)
      val updated = transformObjectTree(m)(v)
      if (updated != m)
        an.set(index, updated)
      an
    } else an
  }

  private def transformObjectNodeField(m: Matcher, fieldName: String)(on: ObjectNode) {
    if (on.has(fieldName)) {
      val v = on.get(fieldName)
      if (v != null) {
        val updated = transformObjectTree(m)(v)
        if (updated != v)
          on.replace(fieldName, updated)
      }
    }
  }

  private def subMatcher(matcher: Option[Matcher], name: String): Option[Matcher] = {
    matcher.flatMap(m => m.subMatchers.find(_.pattern == name).orElse(m.subMatchers.find(_.pattern == "*")))
  }

  private def matchTransformer[A](matcher: Option[Matcher])(select: PartialFunction[Transformer, A]): Option[A] = {
    matchTransformers(matcher)(select).headOption
  }

  private def matchTransformers[A](matcher: Option[Matcher])(select: PartialFunction[Transformer, A]): Seq[A] = {
    matcher.toList.flatMap(_.transformers.collect(select))
  }

  private def transformValue(matcher: Option[Matcher], copy: Boolean)(implicit p: JsonParser, g: JsonGenerator) {
    p.nextToken match {
      case JsonToken.START_OBJECT =>
        transformObject(matcher, copy)
      case JsonToken.START_ARRAY =>
        if (copy) g.writeStartArray()
        var i = 1
        while(p.getCurrentToken != JsonToken.END_ARRAY) {
          val indexMatcher = subMatcher(matcher, i.toString)
          transformValue(indexMatcher, copy)
          i += 1
        }
      case JsonToken.END_ARRAY =>
        if (copy) g.writeEndArray()
      case JsonToken.VALUE_STRING =>
        if (copy) g.writeString(p.getValueAsString)
      case JsonToken.VALUE_NUMBER_INT | JsonToken.VALUE_NUMBER_FLOAT =>
        if (copy) g.writeNumber(p.getText) // using getText to avoid any potential precision loss by converting to int or double
      case JsonToken.VALUE_FALSE | JsonToken.VALUE_TRUE =>
        if (copy) g.writeBoolean(p.getBooleanValue)
      case JsonToken.VALUE_NULL =>
        if (copy) g.writeNull()
      case invalid => sys.error("expected valid value, but got instead " + invalid)
    }
  }

  private def autoCloseStreams[A](is: InputStream, os: OutputStream)(op: => A): A = {
    try {
      op
    } finally {
      try {
        is.close()
      } catch { case _: IOException => }
      try {
        os.close()
      } catch { case _: IOException => }
    }
  }
}

object Transform {

  def main(args: Array[String]) {
    val rename1 = Matcher("results", Seq(Matcher("*", Seq(), Seq(RenameAttribute("oldName", "newName")))))
    val rootMatcher = Matcher("<root>", Seq(rename1))
    // in DSL this could be: "results" / "*" / rename("oldName", "newName") ~
    val t = new Transform(rootMatcher)

    // TODO open a stream for reading and one for writing

  }
}
