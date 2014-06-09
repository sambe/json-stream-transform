package org.github.sambe.jsonstreamtransform

import com.fasterxml.jackson.databind.JsonNode

case class Matcher(
  pattern: String,
  subMatchers: Seq[Matcher],
  transformers: Seq[Transformer] = Seq(),
  condition: Option[JsonNode => Boolean] = None)

// as soon as there is a matcher with condition in the matcher tree, it will trigger fetching the value of the matched
// attribute as a JsonNode. As such the user is urged to use this condition only on matchers that match small objects
// near leaves of the JSON tree