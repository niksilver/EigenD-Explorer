package org.pigsaw.eigendmapper

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.pigsaw.eigendmapper._
import org.pigsaw.eigendmapper.Mapper._

@RunWith(classOf[JUnitRunner])
class DictionarySuite extends FunSuite {

  test("Read dictionary line") {
    new BCatOutputParser {
      assert(parseLine("1 hello") === Some(StateVariableLine("1", "hello")))
      assert(parseLine("12 hello") === Some(StateVariableLine("12", "hello")))
      assert(parseLine("12.34 hello") === Some(StateVariableLine("12.34", "hello")))
      assert(parseLine("12.34.56 hello") === Some(StateVariableLine("12.34.56", "hello")))
      assert(parseLine("something else") === None)
    }
  }

}
