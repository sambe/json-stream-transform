package org.github.sambe.jsonstreamtransform

import org.junit.{Assert, Test}
import java.io.{ByteArrayOutputStream, ByteArrayInputStream}
import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.node.{ArrayNode, ObjectNode}
import scala.collection.JavaConverters.iterableAsScalaIterableConverter

class TransformerTest {

  val inputJson =
    """{
      |  "name" : "Samuel",
      |  "age" : 29,
      |  "married" : false,
      |  "languages" : [ "English", "German", "Spanish", "French" ],
      |  "employer" : {
      |    "name" : "Leonteq Securities AG",
      |    "listed" : true,
      |    "locations" : [ {
      |      "place" : "Zurich",
      |      "employees" : 250
      |    }, {
      |      "place" : "Hong Kong",
      |      "employees" : 25
      |    } ]
      |  }
      |}""".stripMargin

  @Test
  def testNoOp {
    val matcher = new Matcher("<root>", Seq(), Seq())
    val t = new Transform(matcher)

    val outputJson = transform(t, inputJson)
    Assert.assertEquals(inputJson, outputJson)
  }

  @Test
  def testRename {
    val rename1 = RenameAttribute("name", "firstName")
    val rename2 = Matcher("employer", Seq(Matcher("locations", Seq(Matcher("*", Seq(), Seq(RenameAttribute("place", "placeName")))))))
    val matcher = Matcher("<root>", Seq(rename2), Seq(rename1))
    val t = new Transform(matcher)

    val expectedJson = """{
                         |  "firstName" : "Samuel",
                         |  "age" : 29,
                         |  "married" : false,
                         |  "languages" : [ "English", "German", "Spanish", "French" ],
                         |  "employer" : {
                         |    "name" : "Leonteq Securities AG",
                         |    "listed" : true,
                         |    "locations" : [ {
                         |      "placeName" : "Zurich",
                         |      "employees" : 250
                         |    }, {
                         |      "placeName" : "Hong Kong",
                         |      "employees" : 25
                         |    } ]
                         |  }
                         |}""".stripMargin

    val outputJson = transform(t, inputJson)
    Assert.assertEquals(expectedJson, outputJson)
  }

  @Test
  def testRemove {
    val remove1 = RemoveAttribute("age")
    val remove2 = Matcher("employer", Seq(), Seq(RemoveAttribute("locations")))
    val matcher = Matcher("<root>", Seq(remove2), Seq(remove1))
    val t = new Transform(matcher)

    val expectedJson = """{
                         |  "name" : "Samuel",
                         |  "married" : false,
                         |  "languages" : [ "English", "German", "Spanish", "French" ],
                         |  "employer" : {
                         |    "name" : "Leonteq Securities AG",
                         |    "listed" : true
                         |  }
                         |}""".stripMargin

    val outputJson = transform(t, inputJson)
    Assert.assertEquals(expectedJson, outputJson)
  }

  @Test
  def testRemoveLevel {
    val removeLevel = Matcher("employer", Seq(Matcher("locations", Seq(Matcher("*", Seq(), Seq(RemoveLevel("place")))))))
    val matcher = Matcher("<root>", Seq(removeLevel))
    val t = new Transform(matcher)

    val expectedJson = """{
                         |  "name" : "Samuel",
                         |  "age" : 29,
                         |  "married" : false,
                         |  "languages" : [ "English", "German", "Spanish", "French" ],
                         |  "employer" : {
                         |    "name" : "Leonteq Securities AG",
                         |    "listed" : true,
                         |    "locations" : [ "Zurich", "Hong Kong" ]
                         |  }
                         |}""".stripMargin

    val outputJson = transform(t, inputJson)
    Assert.assertEquals(expectedJson, outputJson)
  }

  @Test
  def testInsertAttribute {
    val insert1 = InsertAttribute("criminalRecord", false)
    val insert2 = InsertAttribute("profession", "Software Developer")
    val insert3 = Matcher("employer", Seq(Matcher("locations", Seq(
      Matcher("1", Seq(), Seq(InsertAttribute("countryCode", "CH"))),
      Matcher("2", Seq(), Seq(InsertAttribute("countryCode", "HK")))
    ))))
    val matcher = Matcher("<root>", Seq(insert3), Seq(insert1, insert2))
    val t = new Transform(matcher)

    val expectedJson = """{
                         |  "name" : "Samuel",
                         |  "age" : 29,
                         |  "married" : false,
                         |  "languages" : [ "English", "German", "Spanish", "French" ],
                         |  "employer" : {
                         |    "name" : "Leonteq Securities AG",
                         |    "listed" : true,
                         |    "locations" : [ {
                         |      "place" : "Zurich",
                         |      "employees" : 250,
                         |      "countryCode" : "CH"
                         |    }, {
                         |      "place" : "Hong Kong",
                         |      "employees" : 25,
                         |      "countryCode" : "HK"
                         |    } ]
                         |  },
                         |  "criminalRecord" : false,
                         |  "profession" : "Software Developer"
                         |}""".stripMargin

    val outputJson = transform(t, inputJson)
    Assert.assertEquals(expectedJson, outputJson)
  }

  @Test
  def testConditionalRemove {
    def removeFilter(jsonNode: JsonNode): Boolean = jsonNode match {
      case on: ObjectNode => on.get("employees").asInt() < 50
    }
    val remove = Matcher("employer", Seq(Matcher("locations", Seq(Matcher("*", Seq(), Seq(RemoveAttribute("employees")), Some(removeFilter))))))
    val matcher = Matcher("<root>", Seq(remove))
    val t = new Transform(matcher)

    val expectedJson = """{
                         |  "name" : "Samuel",
                         |  "age" : 29,
                         |  "married" : false,
                         |  "languages" : [ "English", "German", "Spanish", "French" ],
                         |  "employer" : {
                         |    "name" : "Leonteq Securities AG",
                         |    "listed" : true,
                         |    "locations" : [ {
                         |      "place" : "Zurich",
                         |      "employees" : 250
                         |    }, {
                         |      "place" : "Hong Kong"
                         |    } ]
                         |  }
                         |}""".stripMargin

    val outputJson = transform(t, inputJson)
    Assert.assertEquals(expectedJson, outputJson)
  }

  @Test
  def testConditionalRename {
    def renameFilter(jsonNode: JsonNode): Boolean = jsonNode match {
      case on: ObjectNode => on.get("place").asText() != "Zurich"
    }
    val rename = Matcher("employer", Seq(Matcher("locations", Seq(Matcher("*", Seq(), Seq(RenameAttribute("employees", "minions")), Some(renameFilter))))))
    val matcher= Matcher("<root>", Seq(rename))
    val t = new Transform(matcher)

    val expectedJson = """{
                         |  "name" : "Samuel",
                         |  "age" : 29,
                         |  "married" : false,
                         |  "languages" : [ "English", "German", "Spanish", "French" ],
                         |  "employer" : {
                         |    "name" : "Leonteq Securities AG",
                         |    "listed" : true,
                         |    "locations" : [ {
                         |      "place" : "Zurich",
                         |      "employees" : 250
                         |    }, {
                         |      "place" : "Hong Kong",
                         |      "minions" : 25
                         |    } ]
                         |  }
                         |}""".stripMargin

    val outputJson = transform(t, inputJson)
    Assert.assertEquals(expectedJson, outputJson)
  }

  @Test
  def testConditionalInsert {
    def insertFilter(jsonNode: JsonNode): Boolean = jsonNode match {
      case on: ObjectNode => on.get("employees").asInt() > 200
    }
    val insert = Matcher("employer", Seq(Matcher("locations", Seq(Matcher("*", Seq(), Seq(InsertAttribute("crowded", true)), Some(insertFilter))))))
    val matcher = Matcher("<root>", Seq(insert))
    val t = new Transform(matcher)

    val expectedJson = """{
                         |  "name" : "Samuel",
                         |  "age" : 29,
                         |  "married" : false,
                         |  "languages" : [ "English", "German", "Spanish", "French" ],
                         |  "employer" : {
                         |    "name" : "Leonteq Securities AG",
                         |    "listed" : true,
                         |    "locations" : [ {
                         |      "place" : "Zurich",
                         |      "employees" : 250,
                         |      "crowded" : true
                         |    }, {
                         |      "place" : "Hong Kong",
                         |      "employees" : 25
                         |    } ]
                         |  }
                         |}""".stripMargin

    val outputJson = transform(t, inputJson)
    Assert.assertEquals(expectedJson, outputJson)
  }

  @Test
  def testConditionalRemoveLevel {
    // the following condition is quite nonsensical, just to try out a more complicated one too
    def removeLevelFilter(jsonNode: JsonNode): Boolean = jsonNode match {
      case on: ObjectNode => on.get("languages") match {
        case an: ArrayNode => an.asScala.exists(_.asText == "French")
      }
    }
    // removing name of employer before removing level in order to prevent it from overwriting the top level name attribute
    val removeName = Matcher("employer", Seq(), Seq(RemoveAttribute("name")))
    val removeLevel = RemoveLevel("employer")
    val matcher = Matcher("<root>", Seq(removeName), Seq(removeLevel), Some(removeLevelFilter))
    val t = new Transform(matcher)

    val expectedJson = """{
                         |  "name" : "Samuel",
                         |  "age" : 29,
                         |  "married" : false,
                         |  "languages" : [ "English", "German", "Spanish", "French" ],
                         |  "listed" : true,
                         |  "locations" : [ {
                         |    "place" : "Zurich",
                         |    "employees" : 250
                         |  }, {
                         |    "place" : "Hong Kong",
                         |    "employees" : 25
                         |  } ]
                         |}""".stripMargin

    val outputJson = transform(t, inputJson)
    Assert.assertEquals(expectedJson, outputJson)
  }

  private def transform(t: Transform, json: String): String = {
    val is = new ByteArrayInputStream(json.getBytes("utf-8"))
    val os = new ByteArrayOutputStream()
    t.transform(is, os)
    new String(os.toByteArray, "utf-8")
  }
}
