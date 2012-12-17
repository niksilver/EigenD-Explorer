package org.pigsaw.eigendmapper

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers

@RunWith(classOf[JUnitRunner])
class DeprecatedPortSuite extends FunSuite with ShouldMatchers {

  test("Unqualified - general") {
    DeprecatedPort("<a>#1.1", None).unqualified should equal (DeprecatedPort("<a>#1.1", None))
    DeprecatedPort("<main:b>#1.2", None).unqualified should equal (DeprecatedPort("<b>#1.2", None))
  }

  test("Unqualified - FQ agent names in rigs") {
    DeprecatedPort("<main.rig3:summer1>#1.2", None).unqualified should equal (DeprecatedPort("<summer1>#1.2", None))
    DeprecatedPort("<main.rig1:main.rig2:c>#1.2", None).unqualified should equal (DeprecatedPort("<c>#1.2", None))
  }

  test("Unqualified - object preservation") {
    val b = DeprecatedPort("<main:b>#1.2", None).unqualified
    
    assert(b.unqualified eq b)
  }
  
  test("Agent - Port has agent name") {
    DeprecatedPort("<alpha>#1.1", None).agent should equal (Some("<alpha>"))
  }
  
  test("Agent - Port doesn't have agent name") {
    DeprecatedPort("alpha#1.1", None).agent should equal (None)
  }
  
  test("Non-empty name") {
    DeprecatedPort("<alpha>#1.1", None).nonEmptyName should equal ("<alpha>#1.1")
    DeprecatedPort("<alpha>#1.1", Some("bar beat")).nonEmptyName should equal ("bar beat")
  }
  
  test("Non-empty fully-qualified name") {
    DeprecatedPort("<alpha>#1.1", None).nonEmptyFQName should equal ("<alpha>#1.1")
    DeprecatedPort("<alpha>#1.1", Some("bar beat")).nonEmptyFQName should equal ("<alpha> bar beat")
    DeprecatedPort("alpha#1.1", None).nonEmptyFQName should equal ("alpha#1.1")
    DeprecatedPort("alpha#1.1", Some("bar beat")).nonEmptyFQName should equal ("<UNKNOWN> bar beat")
  }
}