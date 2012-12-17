package org.pigsaw.eigendmapper

import Preamble._

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers

@RunWith(classOf[JUnitRunner])
class PreambleSuite extends FunSuite with ShouldMatchers {

  test("AgentName.withoutBrackets") {
    AgentName("<one>").withoutBrackets should equal("one")
    AgentName("one>").withoutBrackets should equal("one")
    AgentName("<one").withoutBrackets should equal("one")
    AgentName("one").withoutBrackets should equal("one")
  }

  test("AgentName.fqName") {
    "<summer1>".fqName(List()) should equal("<summer1>")
    "<summer1>".fqName(List("<rig1>")) should equal("<main.rig1:summer1>")
    "<summer1>".fqName(List("<rig1>", "<rig2>")) should equal("<main.rig1:main.rig2:summer1>")
  }

  test("AgentName.unqualified") {
    "<summer1>".unqualified should equal("<summer1>")
    "<main.rig1:summer1>".unqualified should equal("<summer1>")
    "<main.rig1:main.rig2:summer1>".unqualified should equal("<summer1>")
  }

  test("Pos.index") {
    Pos().index should equal("<main>")
    Pos("<rig1>").index should equal("<main.rig1:main>")
    Pos("<rig1>", "<rig2>").index should equal("<main.rig1:main.rig2:main>")
  }

  test("Pos.displayString") {
    Pos().displayString should equal("Top level")
    Pos("<rig1>").displayString should equal("<rig1>")
    Pos("<rig1>", "<rig2>").displayString should equal("<rig1> - <rig2>")
  }

  test("PortID - Equality") {
    val p1 = PortID("<agent1>#1.2.3")
    val p2 = PortID("<agent1>#1.2.3")
    val p3 = PortID("<agent1>#1.2")

    p1 should equal(p1)
    p1 should equal(p2)
    p1 should not equal (p3)
  }

  test("PortID - Bad input") {
    intercept[IllegalArgumentException] {
      PortID("something").agent
    }
  }
  
  test("PortID - Extract agent name") {
    PortID("<agent1>#1.2.3").agent should equal("<agent1>")
    PortID("<main1.rig1:agent1/1>#1.2.3").agent should equal("<main1.rig1:agent1/1>")
  }

  ignore("PortID - Unqualified agent name") {}
  ignore("PortID - Extract node ID") {}
  ignore("PortID - Extract node cname") {}
  ignore("PortID - Extract node label (ID or cname)") {}
  ignore("PortID - Make it the best format, using cname if possible") {}

}