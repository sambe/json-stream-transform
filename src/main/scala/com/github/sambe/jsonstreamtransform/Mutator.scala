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
import com.fasterxml.jackson.core.TreeNode

sealed trait Mutator

case class ReplaceValue(value: Any) extends Mutator
case class RenameAttribute(oldName: String, newName: String) extends Mutator
case class RemoveAttribute(name: String) extends Mutator
case class InsertAttribute(name: String, value: Any) extends Mutator
case class RemoveLevel(inner: String) extends Mutator
case class InsertLevel(name: String, from: Location, to: Location) extends Mutator // create start object, check if create start object has happened before writing end, check at the end of the object whether it has been closed

// ideas for further mutators
//case class MoveAfter(name: String, to: Location) extends Mutator // create JsonGenerator target at a gzipped byte array and then replay into actual generator
//case class ReplaceWithObject(valueToObject: String => Any) extends Mutator


sealed trait Location

case object Start extends Location
case object End extends Location
case class Before(fieldName: String) extends Location
case class After(fieldName: String) extends Location

// current situation:
// set anyway, risking duplicate (add) => supported (insert), will have to be removed in favor of add-or-update
// set only if exists (update) => not yet supported (tree: yes, stream: no)
// set anyway (add or update) => not yet supported (tree: yes, stream: no)
// set only if not exists (add default) => not yet supported (tree: via condition, stream: no)
// remove => supported
// remove level => not yet supported (tree: no, stream: yes)


// TODO
// - stream processing: add update + awareness of what has been updated (to do AddIfNotExists & AddOrUpdate)
// - make available operations more clear (e.g AddOnlyIfNotExists, AddOrUpdate) only offer nonfailable operations so far
