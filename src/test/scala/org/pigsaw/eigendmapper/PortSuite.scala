package org.pigsaw.eigendmapper

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers

@RunWith(classOf[JUnitRunner])
class PortSuite extends FunSuite with ShouldMatchers {

  test("Unqualified - general") {
    Port("<a>#1.1", None).unqualified should equal (Port("<a>#1.1", None))
    Port("<main:b>#1.2", None).unqualified should equal (Port("<b>#1.2", None))
  }

  test("Unqualified - FQ agent names in rigs") {
    Port("<main.rig3:summer1>#1.2", None).unqualified should equal (Port("<summer1>#1.2", None))
    Port("<main.rig1:main.rig2:c>#1.2", None).unqualified should equal (Port("<c>#1.2", None))
  }

  test("Unqualified - object preservation") {
    val b = Port("<main:b>#1.2", None).unqualified
    
    assert(b.unqualified eq b)
  }
  
  test("Agent - Port has agent name") {
    Port("<alpha>#1.1", None).agent should equal (Some("<alpha>"))
  }
  
  test("Agent - Port doesn't have agent name") {
    Port("alpha#1.1", None).agent should equal (None)
  }
  
  test("Non-empty name") {
    Port("<alpha>#1.1", None).nonEmptyName should equal ("<alpha>#1.1")
    Port("<alpha>#1.1", Some("bar beat")).nonEmptyName should equal ("bar beat")
  }
  
  test("Non-empty fully-qualified name") {
    Port("<alpha>#1.1", None).nonEmptyFQName should equal ("<alpha>#1.1")
    Port("<alpha>#1.1", Some("bar beat")).nonEmptyFQName should equal ("<alpha> bar beat")
    Port("alpha#1.1", None).nonEmptyFQName should equal ("alpha#1.1")
    Port("alpha#1.1", Some("bar beat")).nonEmptyFQName should equal ("<UNKNOWN> bar beat")
  }
}