package org.pigsaw.eigendmapper

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers

@RunWith(classOf[JUnitRunner])
class ConnectionSuite extends FunSuite with ShouldMatchers {

  test("Unqualified - general") {
    val a = "<a>#1.1"
    val a_long = "<main:a>#1.1"
    val b = "<b>#1.2"
    val b_long = "<main:b>#1.2"
    
    Connection(a, b).unqualified should equal (Connection(a, b))
    Connection(a_long, b).unqualified should equal (Connection(a, b))
    Connection(a, b_long).unqualified should equal (Connection(a, b))
    Connection(a_long, b_long).unqualified should equal (Connection(a, b))
  }

  test("Unqualified - object preservation") {
    val a = "<a>#1.1"
    val b = "<b>#1.2"
    
    val a_b = Connection(a, b)

    assert(a_b.unqualified eq a_b)
  }
  
  test("Agents") {
    val a = "<a>#1.1"
    val b = "<b>#1.2"
    
    Connection(a, b).agents should equal (Set("<a>", "<b>"))
  }
  
  test("Connection.hasPos") {
    Connection("<a>#3.2", "<b> beat input").hasPos(List()) should equal (true)
    Connection("<a>#3.2", "<b> beat input").hasPos(List("<rig1>")) should equal (false)
    
    Connection("<main.rig1:a>#3.2", "<b> beat input").hasPos(List()) should equal (true)
    Connection("<main.rig1:a>#3.2", "<b> beat input").hasPos(List("<rig1>")) should equal (true)
    Connection("<a>#3.2", "<main.rig1:b> beat input").hasPos(List("<rig1>")) should equal (true)

    Connection("<main.rig1:a>#3.2", "<main.rig1:b> beat input").hasPos(List()) should equal (false)
  }
}