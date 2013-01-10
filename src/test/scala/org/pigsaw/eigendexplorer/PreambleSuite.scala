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

  test("AgentOrPortID.unqualifiedForPos") {
    "<rig1>".unqualifiedForPos(List("<rig3>")) should equal ("<rig1>")
    
    "<main:rig1>".unqualifiedForPos(List()) should equal ("<rig1>")
    "<main:rig1>".unqualifiedForPos(List("<rig3>")) should equal ("<main:rig1>")
    
    "<main.rig3:cycler1>".unqualifiedForPos(List()) should equal ("<main.rig3:cycler1>")
    "<main.rig3:cycler1>".unqualifiedForPos(List("<rig3>")) should equal ("<cycler1>")
  }

  test("AgentOrPortID.hasPos") {
    "<delay1>".hasPos(List()) should equal(true)
    "<main:delay1>".hasPos(List()) should equal(true)
    
    "<delay1>".hasPos(List("<rig1>")) should equal(false)
    "<main:delay1>".hasPos(List("<rig1>")) should equal(false)

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
    "<main:delay1>".pos should equal (List())
    "<main.rig1:delay1>".pos should equal (List("<rig1>"))
    "<main.rig1:main.rig2:delay1>".pos should equal (List("<rig1>", "<rig2>"))

    // And the same with ports...

    "<delay1>#3.2".pos should equal (List())
    "<main:delay1>#3.2".pos should equal (List())
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
    "<summer1>".qualified(List()) should equal("<main:summer1>")
    "<summer1>".qualified(List("<rig1>")) should equal("<main.rig1:summer1>")
    "<summer1>".qualified(List("<rig1>", "<rig2>")) should equal("<main.rig1:main.rig2:summer1>")
  }

  test("AgentName.defaultQualifier") {
    "<summer1>".defaultQualifier(List()) should equal("<main:summer1>")
    "<summer1>".defaultQualifier(List("<rig1>")) should equal("<main.rig1:summer1>")
    "<summer1>".defaultQualifier(List("<rig1>", "<rig2>")) should equal("<main.rig1:main.rig2:summer1>")

    "<main.rig1:summer1>".defaultQualifier(List()) should equal("<main.rig1:summer1>")
    "<main:summer1>".defaultQualifier(List("<rig1>")) should equal("<main:summer1>")
  }

  test("AgentName.unqualified") {
    "<summer1>".unqualified should equal("<summer1>")
    "<main.rig1:summer1>".unqualified should equal("<summer1>")
    "<main.rig1:main.rig2:summer1>".unqualified should equal("<summer1>")
  }

  test("AgentName.isAgent") {
    "<summer1>".isAgent should equal(true)
    "<main:summer1>".isAgent should equal(true)
    "<main.rig3:summer1>".isAgent should equal(true)

    "summer1".isAgent should equal(false)
    "<summer1".isAgent should equal(false)
    "summer1>".isAgent should equal(false)
    "<summer>1".isAgent should equal(false)
    
    "<main:summer1".isAgent should equal(false)
    "main.rig3:summer1>".isAgent should equal(false)
  }

  test("AgentName.isRig") {
    "<rig1>".isRig should equal(true)
    "<main:rig1>".isRig should equal(true)
    "<main.rig3:rig3>".isRig should equal(true)

    "<main.rig3:summer1>".isRig should equal(false)

    "rig1".isRig should equal(false)
    "<rig1".isRig should equal(false)
    "rig1>".isRig should equal(false)
    "<rig>1".isRig should equal(false)
    
    "<main:rig1".isRig should equal(false)
    "main.rig3:rig1>".isRig should equal(false)
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

  test("PortID.nodeLabel") {
    PortID("<rig3> beat input").nodeLabel should equal("beat input")
    PortID("<rig3>#4.5.6").nodeLabel should equal("4.5.6")
  }

  test("PortID.nodeLabelWithHash") {
    PortID("<rig3> beat input").nodeLabelWithHash should equal("beat input")
    PortID("<rig3>#4.5.6").nodeLabelWithHash should equal("#4.5.6")
  }

  test("Pos.qualifier") {
    List().qualifier should equal("main:")
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