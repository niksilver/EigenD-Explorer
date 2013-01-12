/*
 *  Copyright 2012 Nik Silver.
 *  
 *  This file is part of EigenD Explorer.
 *
 *  EigenD Explorer is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  EigenD Explorer is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with EigenD Explorer.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.pigsaw.eigendexplorer

import Preamble._

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers

@RunWith(classOf[JUnitRunner])
class PreambleSuite extends FunSuite with ShouldMatchers {

  test("Agent.withoutBrackets") {
    Agent("<one>").withoutBrackets should equal("one")
    Agent("one>").withoutBrackets should equal("one")
    Agent("<one").withoutBrackets should equal("one")
    Agent("one").withoutBrackets should equal("one")
  }

  test("Agent.qualified") {
    Agent("<summer1>").qualified(Pos()) should equal (Agent("<main:summer1>"))
    Agent("<summer1>").qualified(Pos("<rig1>")) should equal (Agent("<main.rig1:summer1>"))
    Agent("<summer1>").qualified(Pos("<rig1>", "<rig2>")) should equal (Agent("<main.rig1:main.rig2:summer1>"))
  }

  test("Agent.defaultQualifier") {
    Agent("<summer1>").defaultQualifier(Pos()) should equal (Agent("<main:summer1>"))
    Agent("<summer1>").defaultQualifier(Pos("<rig1>")) should equal (Agent("<main.rig1:summer1>"))
    Agent("<summer1>").defaultQualifier(Pos("<rig1>", "<rig2>")) should equal (Agent("<main.rig1:main.rig2:summer1>"))

    Agent("<main.rig1:summer1>").defaultQualifier(Pos()) should equal (Agent("<main.rig1:summer1>"))
    Agent("<main:summer1>").defaultQualifier(Pos("<rig1>")) should equal (Agent("<main:summer1>"))
  }

  test("Agent.unqualified") {
    Agent("<summer1>").unqualified should equal (Agent("<summer1>"))
    Agent("<main.rig1:summer1>").unqualified should equal (Agent("<summer1>"))
    Agent("<main.rig1:main.rig2:summer1>").unqualified should equal (Agent("<summer1>"))
  }

  test("Agent.unqualified - object preservation") {
    val ag = Agent("<rig1>")

    assert(ag.unqualified eq ag)
  }

  test("Agent.unqualifiedForPos") {
    Agent("<rig1>").unqualifiedForPos(Pos("<rig3>")) should equal (Agent("<rig1>"))
    
    Agent("<main:rig1>").unqualifiedForPos(Pos()) should equal (Agent("<rig1>"))
    Agent("<main:rig1>").unqualifiedForPos(Pos("<rig3>")) should equal (Agent("<main:rig1>"))
    
    Agent("<main.rig3:cycler1>").unqualifiedForPos(Pos()) should equal (Agent("<main.rig3:cycler1>"))
    Agent("<main.rig3:cycler1>").unqualifiedForPos(Pos("<rig3>")) should equal (Agent("<cycler1>"))
  }

  test("Agent.hasPos") {
    Agent("<delay1>").hasPos(Pos()) should equal(true)
    Agent("<main:delay1>").hasPos(Pos()) should equal(true)
    
    Agent("<delay1>").hasPos(Pos("<rig1>")) should equal(false)
    Agent("<main:delay1>").hasPos(Pos("<rig1>")) should equal(false)

    Agent("<main.rig1:delay1>").hasPos(Pos()) should equal(false)
    Agent("<main.rig1:delay1>").hasPos(Pos("<rig1>")) should equal(true)
  }
  
  test("Agent.pos") {
    Agent("<delay1>").pos should equal (Pos())
    Agent("<main:delay1>").pos should equal (Pos())
    Agent("<main.rig1:delay1>").pos should equal (Pos("<rig1>"))
    Agent("<main.rig1:main.rig2:delay1>").pos should equal (Pos("<rig1>", "<rig2>"))
  }

  test("Agent.toString") {
    Agent("<rig1>").toString should equal ("<rig1>")
    Agent("<main:rig1>").toString should equal ("<main:rig1>")
  }

  test("AgentString.matchesRig") {
    "<rig1>".matchesRig should equal(true)
    "<main:rig1>".matchesRig should equal(true)
    "<main.rig3:rig3>".matchesRig should equal(true)

    "<main.rig3:summer1>".matchesRig should equal(false)

    "rig1".matchesRig should equal(false)
    "<rig1".matchesRig should equal(false)
    "rig1>".matchesRig should equal(false)
    "<rig>1".matchesRig should equal(false)
    
    "<main:rig1".matchesRig should equal(false)
    "main.rig3:rig1>".matchesRig should equal(false)
  }

  test("AgentString.matchesAgent") {
    "<summer1>".matchesAgent should equal (true)
    "<main:summer1>".matchesAgent should equal (true)
    "<main.rig3:summer1>".matchesAgent should equal (true)

    "summer1".matchesAgent should equal (false)
    "<summer1".matchesAgent should equal (false)
    "summer1>".matchesAgent should equal (false)
    "<summer>1".matchesAgent should equal (false)
    
    "<main:summer1".matchesAgent should equal (false)
    "main.rig3:summer1>".matchesAgent should equal (false)
  }

  test("Pos.index") {
    Pos().index should equal("<main>")
    Pos("<rig1>").index should equal("<main.rig1:main>")
    Pos("<rig1>", "<rig2>").index should equal("<main.rig1:main.rig2:main>")
  }

  test("Pos.:+") {
    Pos() :+ "<rig1>" should equal (Pos("<rig1>"))
    Pos("<rig1>") :+ "<rig2>" should equal (Pos("<rig1>", "<rig2>"))
    Pos("<rig1>", "<rig2>") :+ "<rig3>" should equal (Pos("<rig1>", "<rig2>", "<rig3>"))
  }

  test("Pos.parent") {
    evaluating {
    	Pos().parent
    } should produce [Exception]
    Pos("<rig1>").parent should equal (Pos())
    Pos("<rig1>", "<rig2>").parent should equal (Pos("<rig1>"))
  }

  test("Pos.last") {
    evaluating {
    	Pos().last
    } should produce [Exception]
    Pos("<rig1>").last should equal (Agent("<rig1>"))
    Pos("<rig1>", "<rig2>").last should equal (Agent("<rig2>"))
  }

  test("Pos.topLevel") {
    Pos().topLevel should equal (true)
    Pos("<rig1>").topLevel should equal (false)
    Pos("<rig1>", "<rig2>").topLevel should equal (false)
  }

  test("Pos.notTopLevel") {
    Pos().notTopLevel should equal (false)
    Pos("<rig1>").notTopLevel should equal (true)
    Pos("<rig1>", "<rig2>").notTopLevel should equal (true)
  }

  test("Pos.displayString") {
    Pos().displayString should equal("Top level")
    Pos("<rig1>").displayString should equal("<rig1>")
    Pos("<rig1>", "<rig2>").displayString should equal("<rig1> - <rig2>")
  }

  test("Pos.length") {
    Pos().length should equal (0)
    Pos("<rig1>").length should equal (1)
    Pos("<rig1>", "<rig2>").length should equal (2)
  }

  test("PortID.constructor") {
    // These should simply not throw MatchErrors
    PortID("<rig1>#1.1").unqualified
    PortID("<main:rig1>#1.1").unqualified
    PortID("<main.rig3:cycler1>#1.1").unqualified
    PortID("<main.rig3:main.rig4:cycler1>#34.5").unqualified
  }

  test("PortID.defaultQualifier") {
    PortID("<summer1>#3.4.5").defaultQualifier(Pos()) should equal (PortID("<main:summer1>#3.4.5"))
    PortID("<summer1>#3.4.5").defaultQualifier(Pos("<rig1>")) should equal (PortID("<main.rig1:summer1>#3.4.5"))
    PortID("<summer1>#3.4.5").defaultQualifier(Pos("<rig1>", "<rig2>")) should equal (PortID("<main.rig1:main.rig2:summer1>#3.4.5"))

    PortID("<main.rig1:summer1>#3.4.5").defaultQualifier(Pos()) should equal (PortID("<main.rig1:summer1>#3.4.5"))
    PortID("<main:summer1>#3.4.5").defaultQualifier(Pos("<rig1>")) should equal (PortID("<main:summer1>#3.4.5"))
  }

  test("PortID.unqualifiedForPos") {
    PortID("<rig1>#1.1").unqualifiedForPos(Pos("<rig3>")) should equal (PortID("<rig1>#1.1"))
    
    PortID("<main:rig1>#1.1").unqualifiedForPos(Pos()) should equal (PortID("<rig1>#1.1"))
    PortID("<main:rig1>#1.1").unqualifiedForPos(Pos("<rig3>")) should equal (PortID("<main:rig1>#1.1"))
    
    PortID("<main.rig3:cycler1>#1.1").unqualifiedForPos(Pos()) should equal (PortID("<main.rig3:cycler1>#1.1"))
    PortID("<main.rig3:cycler1>#1.1").unqualifiedForPos(Pos("<rig3>")) should equal (PortID("<cycler1>#1.1"))
  }

  test("PortID.hasPos") {
    PortID("<delay1>#3.2").hasPos(Pos()) should equal(true)
    PortID("<delay1>#3.2").hasPos(Pos("<rig1>")) should equal(false)
    PortID("<main.rig1:delay1>#3.2").hasPos(Pos()) should equal(false)
    PortID("<main.rig1:delay1>#3.2").hasPos(Pos("<rig1>")) should equal(true)
  }
  
  test("PortID.pos") {
    PortID("<delay1>#3.2").pos should equal (Pos())
    PortID("<main:delay1>#3.2").pos should equal (Pos())
    PortID("<main.rig1:delay1>#3.2").pos should equal (Pos("<rig1>"))
    PortID("<main.rig1:main.rig2:delay1>#3.2").pos should equal (Pos("<rig1>", "<rig2>"))
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
    PortID("<agent1>#1.2.3").agent should equal (Agent("<agent1>"))
    PortID("<main1.rig1:agent1/1>#1.2.3").agent should equal (Agent("<main1.rig1:agent1/1>"))
  }

  test("PortID.unqualified - object preservation") {
    val port = PortID("<rig1>#3.2")

    assert(port.unqualified eq port)
  }

  test("PortID - Convert to format with unqualified agent name") {
    PortID("<a>#1.1").unqualified should equal (PortID("<a>#1.1"))
    PortID("<main:b>#1.2").unqualified should equal (PortID("<b>#1.2"))
    PortID("<main.rig3:summer1>#1.2").unqualified should equal (PortID("<summer1>#1.2"))
    PortID("<main.rig1:main.rig2:c>#1.2").unqualified should equal (PortID("<c>#1.2"))
  }

  test("PortID.nodeLabel") {
    PortID("<rig3> beat input").nodeLabel should equal("beat input")
    PortID("<rig3>#4.5.6").nodeLabel should equal("4.5.6")
  }

  test("PortID.nodeLabelWithHash") {
    PortID("<rig3> beat input").nodeLabelWithHash should equal("beat input")
    PortID("<rig3>#4.5.6").nodeLabelWithHash should equal("#4.5.6")
  }

  test("PortID.toString") {
    PortID("<main:rig3>#4.5").toString should equal ("<main:rig3>#4.5")
    PortID("<main:rig3> beat output").toString should equal ("<main:rig3> beat output")
  }

  test("Pos.qualifier") {
    Pos().qualifier should equal("main:")
    Pos("<rig1>").qualifier should equal("main.rig1:")
    Pos("<rig1>", "<rig2>").qualifier should equal("main.rig1:main.rig2:")
  }

  test("Pos.hasPos") {
    val portTop = PortID("<rig1>#1.1")
    val portRigA = PortID("<main.rig1:ag22>#2.2")
    val portRigB = PortID("<main.rig1:main.rig2:ag33>#3.3")

    portTop.hasPos(Pos()) should be (true)
    portTop.hasPos(Pos("<rig1>")) should be (false)
    portTop.hasPos(Pos("<rig1>", "<rig2>")) should be (false)

    portRigA.hasPos(Pos()) should be (false)
    portRigA.hasPos(Pos("<rig1>")) should be (true)
    portRigA.hasPos(Pos("<rig1>", "<rig2>")) should be (false)

    portRigB.hasPos(Pos()) should be (false)
    portRigB.hasPos(Pos("<rig1>")) should be (false)
    portRigB.hasPos(Pos("<rig1>", "<rig2>")) should be (true)    
  }
  
  test("lessThanStringElt") {
    lessThanStringElt(Left(12), Left(13)) should equal (true)
    lessThanStringElt(Left(13), Left(12)) should equal (false)
    lessThanStringElt(Left(13), Left(13)) should equal (false)

    lessThanStringElt(Right('a'), Right('b')) should equal (true)
    lessThanStringElt(Right('b'), Right('a')) should equal (false)
    lessThanStringElt(Right('b'), Right('b')) should equal (false)

    lessThanStringElt(Left(12), Right('b')) should equal (true)
    lessThanStringElt(Right('b'), Left(12)) should equal (false)
  }
  
  test("lessThanAlphaInts") {
    lessThanAlphaInts("agent1", "agent2") should equal (true)
    lessThanAlphaInts("agent2", "agent10") should equal (true)
    lessThanAlphaInts("agent10", "agent A") should equal (true)
    lessThanAlphaInts("agent A", "agent B") should equal (true)

    lessThanAlphaInts("agent2", "agent1") should equal (false)
    lessThanAlphaInts("agent10", "agent2") should equal (false)
    lessThanAlphaInts("agent A", "agent10") should equal (false)
    lessThanAlphaInts("agent B", "agent A") should equal (false)

    lessThanAlphaInts("agent", "agent") should equal (false)
  }
  
  test("lessThanAlphaInts - Handles excessively large ints") {
    lessThanAlphaInts("123456789012345678901234567890", "2") should equal (false)
  }

}