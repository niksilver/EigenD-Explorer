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
      assert(parseWhole(outputLine, "1 hello") === Some(StateVariableLine("1", "hello")))
      assert(parseWhole(outputLine, "12 hello") === Some(StateVariableLine("12", "hello")))
      assert(parseWhole(outputLine, "12.34 hello") === Some(StateVariableLine("12.34", "hello")))
      assert(parseWhole(outputLine, "12.34.56 hello") === Some(StateVariableLine("12.34.56", "hello")))
      assert(parseWhole(outputLine, "something else") === None)
    }
  }

  test("Read dictionary string") {
    new BCatOutputParser {
      assert(parseWhole(dictionary, "{}") === Some(Map()))
      assert(parseWhole(dictionary, "{ key1:value1 }") === Some(Map("key1" -> "value1")))
      assert(parseWhole(dictionary, "{ key1:value1, key2: value2 }") ===
        Some(Map("key1" -> "value1", "key2" -> "value2")))
      assert(parseWhole(dictionary, "{ aa: bb, cc: dd, ee: ff }") ===
        Some(Map("aa" -> "bb", "cc" -> "dd", "ee" -> "ff")))

      assert(parseWhole(dictionary, "something else") === None)
    }
  }

}
