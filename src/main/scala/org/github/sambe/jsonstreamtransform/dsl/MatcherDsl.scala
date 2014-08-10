package org.github.sambe.jsonstreamtransform.dsl

import org.github.sambe.jsonstreamtransform.{Transform, Mutator, Matcher}
import com.fasterxml.jackson.databind.JsonNode

object MatcherDsl {

  implicit def asSpec(path: String) = {
    MatcherBuilder(path, None)
  }
  
  implicit def asSpec(mutator: Mutator) = {
    MutatorBuilder(mutator, None)
  }

  sealed trait SpecBuilder {
    def build: Matcher
    def toTransform: Transform = new Transform(build)
  }

  sealed trait ParentSpecBuilder extends SpecBuilder {
    def parentSpec: Option[ParentSpecBuilder]
  }
  
  case class MutatorBuilder(mutator: Mutator, parentSpec: Option[MatcherBuilder], condition: Option[JsonNode => Boolean] = None) extends SpecBuilder {
    def build = {
      val (m, t) = extractSpecBuilder(this)
      Matcher("<root>", m, t, condition)
    }
  }
  
  case class MatcherBuilder(pattern: String, parentSpec: Option[MatcherBuilder], condition: Option[JsonNode => Boolean] = None) extends SpecBuilder {
    def /(pattern: String) = {
      MatcherBuilder(pattern, Some(this))
    }
    def /(specs: SpecBuilder*) = {
      GroupMatcherBuilder(specs, Some(this))
    }

    def /(c: Condition) = {
      copy(condition = Some(c.condition))
    }

    def build: Matcher = {
      Matcher("<root>", Seq(createMatcher(this, Seq(), Seq())), Seq(), condition)
    }
  }
  
  case class GroupMatcherBuilder(childSpecs: Seq[SpecBuilder], parentSpec: Option[MatcherBuilder]) extends SpecBuilder {
    
    def build = {
      val (m, t) = extractSpecBuilder(this)
      Matcher("<root>", m, t, None)
    }
  }

  case class ConditionBuilder(condition: Condition)

  case class Condition(condition: JsonNode => Boolean)

  def condition(c: JsonNode => Boolean) = new Condition(c)
  
  def /(pattern: String) = {
    MatcherBuilder(pattern, None)
  }
  
  def /(specs: SpecBuilder*) = {
    GroupMatcherBuilder(specs, None)
  }

  def /(c: Condition) = {
    MatcherBuilder
  }

  private def extractSpecBuilders(specs: Seq[SpecBuilder]): (Seq[Matcher], Seq[Mutator]) = {
    specs.map(extractSpecBuilder).unzip match { case (l, r) => (l.flatten.toList, r.flatten.toList)}
  }

  private def extractSpecBuilder(spec: SpecBuilder): (Seq[Matcher], Seq[Mutator]) = {
    spec match {
      case g: GroupMatcherBuilder =>
        val (m, t) = extractSpecBuilders(g.childSpecs)
        g.parentSpec.map(s => (Seq(createMatcher(s, m, t)), Seq[Mutator]())).getOrElse((m, t))
      case m: MatcherBuilder =>
        (Seq(createMatcher(m, Seq(), Seq())), Seq[Mutator]()) // this doesn't make much sense (no effect because no mutators or group)
      case t: MutatorBuilder =>
        t.parentSpec.map(s => (Seq(createMatcher(s, Seq(), Seq(t.mutator))), Seq())).getOrElse((Seq(), Seq(t.mutator)))
    }
  }
  
  // reverses MatcherBuilder chain into matcher chain
  private def createMatcher(s: MatcherBuilder, childMatchers: Seq[Matcher], mutators: Seq[Mutator]): Matcher = {
    val m = Matcher(s.pattern, childMatchers, mutators, s.condition)
    s.parentSpec.map(createMatcher(_, Seq(m), Seq())).getOrElse(m)
  }
}
