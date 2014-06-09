package org.github.sambe.jsonstreamtransform

import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.node.ObjectNode

object Util {

  def onlyIfObjectNode(tree: JsonNode)(op: ObjectNode => Unit) {
    tree match {
      case on: ObjectNode => op(on)
      case _ => // do nothing
    }
  }

  def safeToInt(s: String): Option[Int] = {
    try {
      Some(s.toInt)
    } catch {
      case _: NumberFormatException => None
    }
  }

}
