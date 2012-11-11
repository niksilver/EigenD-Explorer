package org.pigsaw.eigendmapper

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.pigsaw.eigendmapper.Mapper._

@RunWith(classOf[JUnitRunner])
class MapperSuite extends FunSuite {

  test("Initial test") {
    assert(1 + 1 === 2)
  }
  
  test("Filter stream") {
    val stream = Stream("one", "<two>", "<three>", "four")
    val output = filterAgents(stream)
    assert(!(output contains "one"))
    assert(output contains "two")
    assert(output contains "three")
    assert(!(output contains "four"))
  }

}
