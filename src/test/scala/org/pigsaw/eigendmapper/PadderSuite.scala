package org.pigsaw.eigendmapper

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers

@RunWith(classOf[JUnitRunner])
class UtilsSuite extends FunSuite with ShouldMatchers {

  test("Padder - Basic padding") {
    val data = Seq(
      ("", "middle", "end"),
      ("in", "mid", ""),
      ("in2", "middle2", ""))
    val padder = new Padder(data, " -> ")
    
    padder.output(0) should equal ("       middle  -> end")
    padder.output(1) should equal ("in  -> mid    ")
    padder.output(2) should equal ("in2 -> middle2")
  }

  test("Padder - Splits lines if necessary") {
    val data = Seq(
      ("", "12345678901234", "end"),
      ("in", "mid", ""),
      ("inner", "middle2", ""))
    val padCalc = new PadCalc(12, 3,6,3)
    val padder = new Padder(data, " -> ", padCalc)

    padder.output(0) should equal ("       123456 -> end")
    padder.output(1) should equal ("       789012")
    padder.output(2) should equal ("       34    ")
    padder.output(3) should equal ("in  -> mid   ")
    padder.output(4) should equal ("inn -> middle")
    padder.output(5) should equal ("er     2     ")
  }
  
  test("PadCalc") {
    val calc = new PadCalc(30, 8,8,8)
    
    calc.forSizes(5,5,5, 10,10,10) should equal ((10,10,10))
    calc.forSizes(1,1,1, 1,1,1)    should equal ((1,1,1))
    calc.forSizes(2,2,2, 20,20,20) should equal ((10,10,10))
    calc.forSizes(10,20,20, 20,20,20) should equal ((9,10,10)) // Roughly
    calc.forSizes(20,20,10, 20,20,20) should equal ((10,10,9)) // Roughly
    calc.forSizes(5,5,5, 5,10,10)  should equal ((5,10,10))
  }
}