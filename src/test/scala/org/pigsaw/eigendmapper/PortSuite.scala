package org.pigsaw.eigendmapper

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers

@RunWith(classOf[JUnitRunner])
class PortSuite extends FunSuite with ShouldMatchers {

  test("Normalised - general") {
    Port("<a>#1.1", None).normalised should equal (Port("<a>#1.1", None))
    Port("<main:b>#1.2", None).normalised should equal (Port("<b>#1.2", None))
  }

  test("Normalised - object preservation") {
    val b = Port("<main:b>#1.2", None).normalised
    
    assert(b.normalised eq b)
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
}