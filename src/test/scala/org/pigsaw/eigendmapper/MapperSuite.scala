package org.pigsaw.eigendmapper

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MapperSuite extends FunSuite {
  
  test("Filter stream - sensible case") {
    val bls = new BLs("<main>") {
      override def text = Stream("one", "<two>", "<three>", "four")
    }
    val output = bls.agents
    assert(output.length === 2)
    assert(!(output contains "one"))
    assert(output contains "<two>")
    assert(output contains "<three>")
    assert(!(output contains "four"))
  }

  test("Filter stream - empty case 1") {
    val bls = new BLs("<main>") {
      override def text = Stream("one", "four")
    }
    val output = bls.agents
    assert(output.length === 0)
  }

  test("Filter stream - empty case 2") {
    val bls = new BLs("<main>") {
      override def text = Stream()
    }
    val output = bls.agents
    assert(output.length === 0)
  }
 
  /*test("Real bls output") {
    val bls = new BLs("<main>")
    bls.agents.toList map println
  }*/
 
}
