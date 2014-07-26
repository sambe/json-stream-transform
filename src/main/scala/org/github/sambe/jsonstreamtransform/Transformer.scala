package org.github.sambe.jsonstreamtransform

import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.core.TreeNode

sealed trait Transformer

case class ReplaceValue(value: Any) extends Transformer
case class RenameAttribute(oldName: String, newName: String) extends Transformer
case class RemoveAttribute(name: String) extends Transformer
case class InsertAttribute(name: String, value: Any) extends Transformer
case class RemoveLevel(inner: String) extends Transformer

// current situation:
// set if never exists (add) => supported (insert)
// set if always exists (update) => not yet supported (tree: yes, stream: no)
// set if sometimes exists (add or update) => not yet supported (tree: yes, stream: no)
// set only if not exists (add default) => not yet supported (tree: via condition, stream: no)
// remove => supported
// remove level => not yet supported (tree: no, stream: yes)


// TODO
// - stream processing: add update + awareness of what has been updated (to do AddIfNotExists & AddOrUpdate)
// - make available operations more clear (e.g AddOnlyIfNotExists, AddOrUpdate) only offer nonfailable operations so far

// - add DSL (especially matchers + conditions)