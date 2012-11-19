package org.pigsaw.eigendmapper

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers

@RunWith(classOf[JUnitRunner])
class ConnectionSuite extends FunSuite with ShouldMatchers {

  test("Normalised - general") {
    val a = Port("<a>#1.1", None)
    val a_long = Port("<main:a>#1.1", None)
    val b = Port("<b>#1.2", None)
    val b_long = Port("<main:b>#1.2", None)
    
    Connection(a, b).normalised should equal (Connection(a, b))
    Connection(a_long, b).normalised should equal (Connection(a, b))
    Connection(a, b_long).normalised should equal (Connection(a, b))
    Connection(a_long, b_long).normalised should equal (Connection(a, b))
  }

  test("Normalised - object preservation") {
    val a = Port("<a>#1.1", None)
    val a_long = Port("<main:a>#1.1", None)
    val b = Port("<b>#1.2", None)
    val b_long = Port("<main:b>#1.2", None)
    
    val a_b = Connection(a, b)

    assert(a_b.normalised eq a_b)
  }
  
  test("Agents") {
    val a = Port("<a>#1.1", None)
    val b = Port("<b>#1.2", None)
    val c = Port("c>#1.3", None) // No parseable agent name
    
    Connection(a, b).agents should equal (Set("<a>", "<b>"))
    Connection(a, c).agents should equal (Set("<a>"))
    Connection(c, c).agents should equal (Set())
  }
}