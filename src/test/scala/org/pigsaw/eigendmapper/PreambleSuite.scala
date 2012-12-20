package org.pigsaw.eigendmapper

import Preamble._

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers

@RunWith(classOf[JUnitRunner])
class PreambleSuite extends FunSuite with ShouldMatchers {

  test("AgentOrPortID - constructor") {
    // These should simply not throw MatchErrors
    AgentOrPortID("<rig1>").unqualified
    AgentOrPortID("<main:rig1>").unqualified
    AgentOrPortID("<main.rig3:cycler1>").unqualified
    AgentOrPortID("<main.rig3:main.rig4:cycler1>#34.5").unqualified
  }

  test("AgentOrPortID - object preservation") {
    val port = "<rig1>#3.2"

    assert(port.unqualified eq port)
  }

  test("AgentOrPortID.hasPos") {
    "<delay1>".hasPos(List()) should equal(true)
    "<delay1>".hasPos(List("<rig1>")) should equal(false)
    "<main.rig1:delay1>".hasPos(List()) should equal(false)
    "<main.rig1:delay1>".hasPos(List("<rig1>")) should equal(true)

    // And the same with ports...

    "<delay1>#3.2".hasPos(List()) should equal(true)
    "<delay1>#3.2".hasPos(List("<rig1>")) should equal(false)
    "<main.rig1:delay1>#3.2".hasPos(List()) should equal(false)
    "<main.rig1:delay1>#3.2".hasPos(List("<rig1>")) should equal(true)
  }
  
  test("AgentOrPortID.pos") {
    "<delay1>".pos should equal (List())
    "<main.rig1:delay1>".pos should equal (List("<rig1>"))
    "<main.rig1:main.rig2:delay1>".pos should equal (List("<rig1>", "<rig2>"))

    // And the same with ports...

    "<delay1>#3.2".pos should equal (List())
    "<main.rig1:delay1>#3.2".pos should equal (List("<rig1>"))
    "<main.rig1:main.rig2:delay1>#3.2".pos should equal (List("<rig1>", "<rig2>"))
  }

  test("AgentName.withoutBrackets") {
    AgentName("<one>").withoutBrackets should equal("one")
    AgentName("one>").withoutBrackets should equal("one")
    AgentName("<one").withoutBrackets should equal("one")
    AgentName("one").withoutBrackets should equal("one")
  }

  test("AgentName.qualified") {
    "<summer1>".qualified(List()) should equal("<summer1>")
    "<summer1>".qualified(List("<rig1>")) should equal("<main.rig1:summer1>")
    "<summer1>".qualified(List("<rig1>", "<rig2>")) should equal("<main.rig1:main.rig2:summer1>")
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
    // General bad format
    intercept[MatchError] { PortID("something") }

    // No agent (1)
    intercept[MatchError] { PortID("one#3.2.4") }

    // No agent (2)
    intercept[MatchError] { PortID("#3.2.4") }

    // No node label (1)
    intercept[MatchError] { PortID("<agent>#") }

    // No node label (2)
    intercept[MatchError] { PortID("<agent>") }

    // No separator 
    intercept[MatchError] { PortID("<agent>3.4.5") }
  }

  test("PortID - Extract agent name") {
    PortID("<agent1>#1.2.3").agent should equal("<agent1>")
    PortID("<main1.rig1:agent1/1>#1.2.3").agent should equal("<main1.rig1:agent1/1>")
  }

  test("PortID - Convert to format with unqualified agent name") {
    "<a>#1.1".unqualified should equal("<a>#1.1")
    "<main:b>#1.2".unqualified should equal("<b>#1.2")
    "<main.rig3:summer1>#1.2".unqualified should equal("<summer1>#1.2")
    "<main.rig1:main.rig2:c>#1.2".unqualified should equal("<c>#1.2")
  }

  test("PortID - Extract node label (ID or cname)") {
    PortID("<rig3> beat input").nodeLabel should equal("beat input")
    PortID("<rig3>#4.5.6").nodeLabel should equal("4.5.6")
  }

  test("PortID - Make it the best format, using cname if possible") {
    val map = Map(
      "1" -> "one input",
      "2.3" -> "two output")

    PortID("<cycler1>#1").bestForm(map) should equal("<cycler1> one input")
    PortID("<cycler1>#2.3").bestForm(map) should equal("<cycler1> two output")

    PortID("<cycler1>#1.1").bestForm(map) should equal("<cycler1>#1.1")
    PortID("<cycler1>#2").bestForm(map) should equal("<cycler1>#2")
  }

  test("Pos.qualifier") {
    List().qualifier should equal("")
    List("<rig1>").qualifier should equal("main.rig1:")
    List("<rig1>", "<rig2>").qualifier should equal("main.rig1:main.rig2:")
  }

  test("Pos.hasPos") {
    val portTop = "<rig1>#1.1"
    val portRigA = "<main.rig1:ag22>#2.2"
    val portRigB = "<main.rig1:main.rig2:ag33>#3.3"

    portTop.hasPos(List()) should be (true)
    portTop.hasPos(List("<rig1>")) should be (false)
    portTop.hasPos(List("<rig1>", "<rig2>")) should be (false)

    portRigA.hasPos(List()) should be (false)
    portRigA.hasPos(List("<rig1>")) should be (true)
    portRigA.hasPos(List("<rig1>", "<rig2>")) should be (false)

    portRigB.hasPos(List()) should be (false)
    portRigB.hasPos(List("<rig1>")) should be (false)
    portRigB.hasPos(List("<rig1>", "<rig2>")) should be (true)    
  }

}