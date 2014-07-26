package org.github.sambe.jsonstreamtransform

import org.junit.{Assert, Test}
import java.io.{ByteArrayOutputStream, ByteArrayInputStream}
import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.node.{ArrayNode, ObjectNode}
import scala.collection.JavaConverters.iterableAsScalaIterableConverter
import org.github.sambe.jsonstreamtransform.dsl.MatcherDsl._

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
    val t = / (
      RenameAttribute("name", "firstName"),
      "employer" / "locations" / "*" / RenameAttribute("place", "placeName")
    ).toTransform

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
    val t = /(
      RemoveAttribute("age"),
      "employer" / RemoveAttribute("locations")
    ).toTransform

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
    val t = /( "employer" / "locations" / "*" / RemoveLevel("place")).toTransform

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
    val t = /(
      InsertAttribute("criminalRecord", false),
      InsertAttribute("profession", "Software Developer"),
      "employer" / "locations" / (
        "1" / InsertAttribute("countryCode", "CH"),
        "2" / InsertAttribute("countryCode", "HK")
        )
    ).toTransform

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
    val t = /(
      "employer" / "locations" / "*" / condition(_.get("employees").asInt() < 50) / (
        RemoveAttribute("employees")
      )
    ).toTransform

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
    val t = /(
      "employer" / "locations" / "*" / condition(_.get("place").asText != "Zurich") / RenameAttribute("employees", "minions")
    ).toTransform

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
    val t = /(
      "employer" / "locations" / "*" / condition(_.get("employees").asInt() > 200) /InsertAttribute("crowded", true)
    ).toTransform

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

    // This example has a condition on the root matcher which is normally not supported
    val t = new Transform(/ (
      "employer" / RemoveAttribute("name"),
      RemoveLevel("employer")
    ).build.copy(condition = Some(removeLevelFilter)))

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
