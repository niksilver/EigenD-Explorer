package org.pigsaw.eigendmapper

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers

import org.pigsaw.eigendmapper.Graphable._

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
}