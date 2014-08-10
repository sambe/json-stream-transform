# Transforming JSON Stream-Based

Json-stream-transform is a Scala-based library to apply changes to the structure of a JSON document, while streaming it
from an input to an output stream. It is an ideal basis to implement an automated migration for JSON documents in any
system using Scala. There is also a Scala-based DSL for specifying the structural changes in a self-documenting,
declarative style.

## Does the following apply to you?
- You have persisted JSON data and you'd like to migrate it along with the application?
- Simple solutions don't work because some of the JSON documents are too big to keep in memory?
- And you don't like to write custom migration code each time a little detail changes in your data model.
- You use Scala (or you are not afraid of it)

**Then json-stream-transform might be the core component to solve your problem.**

## Json-stream-transform is...
- Easy to use, thanks to its DSL for specifying the mutations
- Easy to integrate, pure Scala code
- Based on fast Jackson JSON parser and writer
- It applies all modifications while streaming through the document
- Suitable for very large JSON documents (>100MB), which cannot be processed by other tools that parse the whole document into a tree.

## Example 1: Renaming two attributes

A transform specified in the DSL, contains mutations like RenameAttribute, which are Scala case classes. And it also
contains information where to apply the mutation. E.g. the first RenameAttribute in below example is applied on the top
level, while the second is applied only inside the given path, where "*" can refer to all members on a level or in this
case, mean that the mutation shall be applied to all elements of an array.

```scala
    val t = / (
      RenameAttribute("name", "firstName"),
      "employer" / "locations" / "*" / RenameAttribute("place", "placeName")
    ).toTransform
    
    t.transform(inputStream, outputStream)
```

For many more examples, please  [check out the unit tests](https://github.com/sambe/json-stream-transform/blob/master/src/test/scala/com/github/sambe/jsonstreamtransform/TransformerTest.scala)!