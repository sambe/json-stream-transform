package org.github.sambe.jsonstreamtransform.dsl

import org.junit.{Assert, Test}
import org.github.sambe.jsonstreamtransform._
import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.node.{ArrayNode, ObjectNode}
import scala.collection.JavaConverters.iterableAsScalaIterableConverter

class TransformerDslTest {

  import MatcherDsl._

  @Test
  def testSimple {
    val rename1 = RenameAttribute("name", "firstName")
    val rename2 = Matcher("employer", Seq(Matcher("locations", Seq(Matcher("*", Seq(), Seq(RenameAttribute("place", "placeName")))))))
    val matcher = Matcher("<root>", Seq(rename2), Seq(rename1))

    val matcherDsl = /(
      RenameAttribute("name", "firstName"),
      "employer" / "locations" / "*" / RenameAttribute("place", "placeName")
    ).build

    Assert.assertEquals(matcher, matcherDsl)
  }

  @Test
  def testRename {
    val rename1 = RenameAttribute("name", "firstName")
    val rename2 = Matcher("employer", Seq(Matcher("locations", Seq(Matcher("*", Seq(), Seq(RenameAttribute("place", "placeName")))))))
    val matcher = Matcher("<root>", Seq(rename2), Seq(rename1))

    val matcherDsl = / (
      RenameAttribute("name", "firstName"),
      "employer" / "locations" / "*" / RenameAttribute("place", "placeName")
    ).build

    Assert.assertEquals(matcher, matcherDsl)
  }

  @Test
  def testRemove {
    val remove1 = RemoveAttribute("age")
    val remove2 = Matcher("employer", Seq(), Seq(RemoveAttribute("locations")))
    val matcher = Matcher("<root>", Seq(remove2), Seq(remove1))

    val matcherDsl = /(
      RemoveAttribute("age"),
      "employer" / RemoveAttribute("locations")
    ).build

    Assert.assertEquals(matcher, matcherDsl)
  }

  @Test
  def testRemoveLevel {
    val removeLevel = Matcher("employer", Seq(Matcher("locations", Seq(Matcher("*", Seq(), Seq(RemoveLevel("place")))))))
    val matcher = Matcher("<root>", Seq(removeLevel))

    val matcherDsl = /( "employer" / "locations" / "*" / RemoveLevel("place")).build

    Assert.assertEquals(matcher, matcherDsl)
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

    val matcherDsl = /(
      InsertAttribute("criminalRecord", false),
      InsertAttribute("profession", "Software Developer"),
      "employer" / "locations" / (
        "1" / InsertAttribute("countryCode", "CH"),
        "2" / InsertAttribute("countryCode", "HK")
        )
    ).build

    Assert.assertEquals(matcher, matcherDsl)
  }

  @Test
  def testConditionalRemove {
    val removeFilter: JsonNode => Boolean = _ match {
      case on: ObjectNode => on.get("employees").asInt() < 50
    }
    val remove = Matcher("employer", Seq(Matcher("locations", Seq(Matcher("*", Seq(), Seq(RemoveAttribute("employees")), Some(removeFilter))))))
    val matcher = Matcher("<root>", Seq(remove))

    val matcherDsl = /(
      "employer" / "locations" / "*" / condition(removeFilter) / (
        RemoveAttribute("employees")
        )
    ).build

    Assert.assertEquals(matcher, matcherDsl)
  }

  @Test
  def testConditionalRename {
    val renameFilter: JsonNode => Boolean = _ match {
      case on: ObjectNode => on.get("place").asText() != "Zurich"
    }
    val rename = Matcher("employer", Seq(Matcher("locations", Seq(Matcher("*", Seq(), Seq(RenameAttribute("employees", "minions")), Some(renameFilter))))))
    val matcher= Matcher("<root>", Seq(rename))

    val matcherDsl = /(
      "employer" / "locations" / "*" / condition(renameFilter) / RenameAttribute("employees", "minions")
    ).build

    Assert.assertEquals(matcher, matcherDsl)
  }

  @Test
  def testConditionalInsert {
    val insertFilter: JsonNode => Boolean = _ match {
      case on: ObjectNode => on.get("employees").asInt() > 200
    }
    val insert = Matcher("employer", Seq(Matcher("locations", Seq(Matcher("*", Seq(), Seq(InsertAttribute("crowded", true)), Some(insertFilter))))))
    val matcher = Matcher("<root>", Seq(insert))

    val matcherDsl = /(
      "employer" / "locations" / "*" / condition(insertFilter) / InsertAttribute("crowded", true)
    ).build

    Assert.assertEquals(matcher, matcherDsl)
  }

  @Test
  def testConditionalRemoveLevel {
    // the following condition is quite nonsensical, just to try out a more complicated one too
    val removeLevelFilter: JsonNode => Boolean = _ match {
      case on: ObjectNode => on.get("languages") match {
        case an: ArrayNode => an.asScala.exists(_.asText == "French")
      }
    }
    // removing name of employer before removing level in order to prevent it from overwriting the top level name attribute
    val removeName = Matcher("employer", Seq(), Seq(RemoveAttribute("name")))
    val removeLevel = RemoveLevel("employer")
    val matcher = Matcher("<root>", Seq(removeName), Seq(removeLevel), Some(removeLevelFilter))

    // This example has a condition on the root matcher which shouldn't normally be done anyway (so making it difficult is okay)
    val matcherDsl = / (
      "employer" / RemoveAttribute("name"),
      RemoveLevel("employer")
    ).build.copy(condition = Some(removeLevelFilter))

    Assert.assertEquals(matcher, matcherDsl)
  }
}
