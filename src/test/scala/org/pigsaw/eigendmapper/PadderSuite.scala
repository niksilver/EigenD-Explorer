package org.pigsaw.eigendmapper

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers

@RunWith(classOf[JUnitRunner])
class UtilsSuite extends FunSuite with ShouldMatchers {

  test("Padder") {
    val data = Seq(
      ("", "middle", "end"),
      ("in", "mid", ""),
      ("in2", "middle2", ""))
    val padder = new Padder(data, " -> ")
    padder.output(0) should equal ("       middle  -> end")
    padder.output(1) should equal ("in  -> mid        ")
    padder.output(2) should equal ("in2 -> middle2    ")
  }
}