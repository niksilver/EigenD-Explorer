package org.pigsaw.eigendmapper

import Preamble._

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers

@RunWith(classOf[JUnitRunner])
class PreambleSuite extends FunSuite with ShouldMatchers {
  
  test("AgentName.withoutBrackets") {
    AgentName("<one>").withoutBrackets should equal ("one")
    AgentName("one>").withoutBrackets should equal ("one")
    AgentName("<one").withoutBrackets should equal ("one")
    AgentName("one").withoutBrackets should equal ("one")
  }
  
  test("Pos.index") {
    Pos().index should equal ("<main>")
    Pos("<rig1>").index should equal ("<main.rig1:main>")
    Pos("<rig1>", "<rig2>").index should equal ("<main.rig1:main.rig2:main>")
  }

}