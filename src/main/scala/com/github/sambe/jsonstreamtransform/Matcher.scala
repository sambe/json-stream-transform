/*
json-stream-transform
Copyright (c) 2014, Samuel Berner, All rights reserved.

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 3.0 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library.
*/

package com.github.sambe.jsonstreamtransform

import com.fasterxml.jackson.databind.JsonNode

case class Matcher(
  pattern: String,
  subMatchers: Seq[Matcher],
  mutators: Seq[Mutator] = Seq(),
  condition: Option[JsonNode => Boolean] = None)

// as soon as there is a matcher with condition in the matcher tree, it will trigger fetching the value of the matched
// attribute as a JsonNode. As such the user is urged to use this condition only on matchers that match small objects
// near leaves of the JSON tree