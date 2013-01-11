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

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers

import Preamble._

@RunWith(classOf[JUnitRunner])
class SetupSuite extends FunSuite with ShouldMatchers {
  
  // Some convenience implicits to make writing tests easier
  
  implicit def mapStringString2MapPortIDPortID(m: Map[String, String]) =
    m map { p => (PortID(p._1), PortID(p._2)) }
  
  implicit def mapStringString2MapPortIDString(m: Map[String, String]) =
    m map { p => (PortID(p._1), p._2) }
  
  test("conns - Just gets top level if at top level") {
    val connTop = Connection("<main:rig1>#1.1", "<main:ag1>#1.1")
    val connRig = Connection("<main.rig1:ag22>#2.2", "<main.rig1:ag23>#2.3")
    
    val setup = Setup(Set(connTop, connRig))
    
    setup.conns should equal (Set(connTop))
  }
  
  test("conns - Just gets lower level unqualified at lower pos") {
    val connTop = Connection("<main:rig1>#1.1", "<main:ag1>#1.1")
    val connRig = Connection("<main.rig1:ag22>#2.2", "<main.rig1:ag23>#2.3")
    
    val setup = Setup(Set(connTop, connRig))
    
    setup.conns(Pos("<rig1>")) should equal (Set(connRig))
  }
  
  test("conns - Without an argument gives connections at pos") {
    val connTop = Connection("<main:rig1>#1.1", "<main:ag1>#1.1")
    val connRig = Connection("<main.rig1:ag22>#2.2", "<main.rig1:ag23>#2.3")
    
    val setup1 = Setup(Set(connTop, connRig)).withPosUpdated(List())
    setup1.conns should equal (Set(connTop))
    
    val setup2 = setup1.withPosUpdated(Pos("<rig1>"))
    setup2.conns should equal (Set(connRig))
  }
  
  test("Conns - Connections come out without best names") {
    val connTop = Connection("<rig1>#1.1", "<ag1>#1.1")
    val connRig = Connection("<main.rig1:ag22>#2.2", "<main.rig1:ag23>#2.3")
    
    val portNames = Map(
        "<rig1>#1.1" -> "<rig1> oneone",
        "<ag1>#1.1" -> "<ag1> agone agone",
        "<main.rig1:ag22>#2.2" -> "<main.rig1:ag22> twotwo",
        "<main.rig1:ag23>#2.3" -> "<main.rig1:ag23> two three")

    val connTopQual = Connection("<main:rig1>#1.1", "<main:ag1>#1.1")

    val setup = Setup(Set(connTop, connRig)).
      withPortNamesReplaced(portNames)
    
    setup.conns(Pos()) should equal (Set(connTopQual))
    setup.conns(Pos("<rig1>")) should equal (Set(connRig))
  }
  
  test("allConns - Gets all connections, fully qualified") {
    val connTop = Connection("<rig1>#1.1", "<ag1>#1.1")
    val connRig = Connection("<main.rig1:ag22>#2.2", "<main.rig1:ag23>#2.3")

    val connTopQual = Connection("<main:rig1>#1.1", "<main:ag1>#1.1")

    val setup = Setup(Set(connTop, connRig))
    
    setup.allConns should equal (Set(connTopQual, connRig))
  }
  
  test("allConns - Defaults to correct rig/pos") {
    val conn = Connection("<rig1>#1.1", "<ag1>#1.1")
    
    val setup = Setup(Set(conn)).withPosUpdated(Pos("<rig8>"))
    
    setup.allConns should equal (Set(Connection("<main:rig1>#1.1", "<main:ag1>#1.1")))
  }
  
  test("allPortNames - Expect port names to be fully qualified") {
    val portNames = Map(
        "<rig1>#1.1" -> "<rig1> oneone",
        "<ag1>#1.1" -> "<ag1> agone agone",
        "<main.rig1:ag22>#2.2" -> "<main.rig1:ag22> twotwo",
        "<main.rig1:ag23>#2.3" -> "<main.rig1:ag23> two three")
    val portNamesQual = Map(
        "<main:rig1>#1.1" -> "<main:rig1> oneone",
        "<main:ag1>#1.1" -> "<main:ag1> agone agone",
        "<main.rig1:ag22>#2.2" -> "<main.rig1:ag22> twotwo",
        "<main.rig1:ag23>#2.3" -> "<main.rig1:ag23> two three")
    
    val setup = Setup().withPortNamesReplaced(portNames)
    
    setup.allPortNames should equal (portNamesQual)
  }
  
  test("portIDNamed - Maps names which are and aren't specified") {
    val portNames = Map(
        "<rig1>#1.1" -> "<rig1> oneone",
        "<main.rig1:ag22>#2.2" -> "<main.rig1:ag22> twotwo")
    
    val setup = Setup().withPortNamesReplaced(portNames)
    
    // Specified
    setup.portIDNamed("<main:rig1>#1.1") should equal ("<main:rig1> oneone")
    setup.portIDNamed("<main.rig1:ag22>#2.2") should equal ("<main.rig1:ag22> twotwo")
    
    // Not specified; should default
    setup.portIDNamed("<main:ag1>#1.1") should equal ("<main:ag1>#1.1")
    setup.portIDNamed( "<main.rig1:ag23>#2.3") should equal ("<main.rig1:ag23>#2.3")
  }

  test("allSettings - Defaults unqualified ports to current pos") {
    val settings = Map(
        "<aaa>#1.1" -> "my triple a value",
        "<main:bbb>#2.2" -> "two dot two")
    val setup = Setup().withPosUpdated(Pos("<rig1>")).withSettingsReplaced(settings)
    
    setup.allSettings should contain (PortID("<main.rig1:aaa>#1.1") -> "my triple a value")
    setup.allSettings should contain (PortID("<main:bbb>#2.2") -> "two dot two")
  }
  
  test("Agents") {
    val a = "<a>#1.1"
    val b = "<b>#1.2"
    val c = "<c>#1.3"

    val setup = new Setup(Set(
      Connection(a, b),
      Connection(b, c)))

    val agents = setup.agents

    agents.size should equal(3)
    agents should contain (Agent("<main:a>"))
    agents should contain (Agent("<main:b>"))
    agents should contain (Agent("<main:c>"))
  }

  test("Ports") {
    val a = PortID("<main:a>#1.1")
    val b = PortID("<main:b>#1.2")
    val c = PortID("<main:c>#1.3")

    val setup = new Setup(Set(
      Connection(a, b),
      Connection(b, c)))

    val ports = setup.ports

    ports.size should equal(3)
    ports should contain (a)
    ports should contain (b)
    ports should contain (c)
  }

  test("Port IDs are automatically qualified") {
    val aUnqual = "<a>#1.1"
    val aQual = "<main:a>#1.1"
    val bUnqual = "<b> b12"
    val bQual = "<main:b> b12"
    val cUnqual = "<c>#1.3"
    val cQual = "<main:c>#1.3"

    val conns = Set(
      Connection(aUnqual, bQual),
      Connection(bQual, cQual),
      Connection(bUnqual, cQual))

    val conns2 = new Setup(conns).conns

    conns2.size should equal(2)
    conns2 should not contain (Connection(aUnqual, bQual))
    conns2 should not contain (Connection(bUnqual, cQual))
    conns2 should contain(Connection(aQual, bQual))
    conns2 should contain(Connection(bQual, cQual))

  }

  test("Apply - Should allow empty setup with no params") {
    val setup = Setup()
    setup.agents.size should be(0)
  }

  test("Rigs - Detects rig names") {
    val conn1 = Connection("<rig3> three", "<rig5> five")
    val conn2 = Connection("<rig7>#7.7", "<other> other")
    val setup = Setup(Set(conn1, conn2))

    setup.rigs.size should equal(3)
    setup.rigs should contain (Agent("<rig3>"))
    setup.rigs should contain (Agent("<rig5>"))
    setup.rigs should contain (Agent("<rig7>"))
  }

  test("Rigs - Detects disconnected rigs") {
    val conn1 = Connection("<ag3> three", "<ag5> five")
    val connRigA = Connection("<main.rig1:ag7>#7.7", "<main.rig1:other> other")
    val connRigB = Connection("<main.rig1:main.rig2:ag9>#9.9", "<main.rig1:main.rig2:other> other")
    val setup = Setup(Set(conn1, connRigA, connRigB))

    setup.rigs should equal (Set(Agent("<rig1>")))
    setup.rigs(Pos("<rig1>")) should equal (Set(Agent("<rig2>")))
  }
  
  test("Rigs - Defaults to current pos") {
    val connTop = Connection("<main:rig1>#1.1", "<main:ag1>#1.1")
    val connRig = Connection("<main.rig1:rig2>#2.2", "<main.rig1:ag23>#2.3")
    
    val setup1 = Setup(Set(connTop, connRig)).withPosUpdated(List())
    setup1.rigs should equal (Set("<rig1>"))
    
    val setup2 = setup1.withPosUpdated(Pos("<rig1>"))
    setup2.rigs should equal (Set("<rig2>"))
  }
  
  test("Position - Can update position") {
    val conn = Connection("<main:rig1> one", "<main:fff> five")
    val setup = new Setup(Set(conn))
    
    setup.pos should equal (List())
    
    val setupV2 = setup.withPosUpdated(Pos("<rig1>"))
    
    setupV2.pos should equal (List("<rig1>"))
    setupV2.allConns should equal (Set(conn))
  }
  
  test("withPortNamesReplaced - Make sure it replaces") {
    val portNames1 = Map(
        "<main:rig1>#1.1" -> "<main:rig1> oneone",
        "<main:ag1>#1.1" -> "<main:ag1> agone agone")
    val portNames2 = Map(
        "<main:rig2>#2.2" -> "<main:rig2> twotwo",
        "<main:ag2>#2.2" -> "<main:ag2> agtwo agtwo")
    
    val setup1 = Setup().withPortNamesReplaced(portNames1)
    
    setup1.allPortNames should equal (portNames1)
    
    val setup2 = setup1.withPortNamesReplaced(portNames2)
    
    setup2.allPortNames should equal (portNames2)
  }
  
  test("withPortNamesReplaced - Defaults pos to current pos") {
    val portNames1 = Map(
        "<rig1>#1.1" -> "<main:rig1> oneone",
        "<main:ag1>#1.1" -> "<ag1> agone agone")
    val portNames2 = Map(
        "<main:rig2>#2.2" -> "<main:rig2> twotwo",
        "<main:ag2>#2.2" -> "<main:ag2> agtwo agtwo")
    
    val setup1 = Setup().withPosUpdated(Pos("<rig2>")).withPortNamesReplaced(portNames1)
    
    setup1.allPortNames should equal (
        Map(
        "<main.rig2:rig1>#1.1" -> "<main:rig1> oneone",
        "<main:ag1>#1.1" -> "<main.rig2:ag1> agone agone")
    )
  }
  
  test("withPortNamesRemoved - Removes port names") {
    val portNames = Map(
        "<main:rig1>#1.1" -> "<main:rig1> oneone",
        "<main:ag1>#1.1" -> "<main:ag1> agone agone",
        "<main.rig1:ag22>#2.2" -> "<main.rig1:ag22> twotwo",
        "<main.rig1:xx23>#2.3" -> "<main.rig1:xx23> two three")

    val setup1 = Setup().withPortNames(portNames)
    
    val test = { portID: PortID => portID.unqualified.toString startsWith "<ag" }
    val setup2 = setup1.withPortNamesRemoved(test)
    
    val expectedPortNames = Map(
        "<main:rig1>#1.1" -> "<main:rig1> oneone",
        "<main.rig1:xx23>#2.3" -> "<main.rig1:xx23> two three")
        
    setup2.allPortNames should equal (expectedPortNames)
  }
  
  test("withPortNamesRemoved - Retains unqualified agent names correctly") {
    val conn = Connection("<rig1>#1.1", "<ag1>#1.1")

    // The unqualified port names should default to the current pos
    // which is initially the top level.
    
    val setup1 = Setup(Set(conn)).withPosUpdated(Pos("<rig9>"))
    val setup2 = setup1.withPortNamesRemoved({ _ => true })
    
    val expectedConn = Connection("<main:rig1>#1.1", "<main:ag1>#1.1")
    
    setup2.allConns should equal (Set(expectedConn))
  }
  
  test("withPortNames - Make sure it adds port names") {
    val portNames1 = Map(
        "<main:rig1>#1.1" -> "<main:rig1> oneone",
        "<main:ag1>#1.1" -> "<main:ag1> agone agone")
    val portNames2 = Map(
        "<main:rig2>#2.2" -> "<main:rig2> twotwo",
        "<main:ag2>#2.2" -> "<main:ag2> agtwo agtwo")
    
    val setup1 = Setup().withPortNamesReplaced(portNames1)
    
    setup1.allPortNames should equal (portNames1)
    
    val setup2 = setup1.withPortNames(portNames2)
    
    setup2.allPortNames should equal (portNames1 ++ portNames2)
  }
  
  test("withPortNames - Defaults unqualified ports to current pos") {
    val portNames1 = Map(
        "<main:rig1>#1.1" -> "<main:rig1> oneone",
        "<main:ag1>#1.1" -> "<main:ag1> agone agone")
    val portNames2 = Map(
        "<rig2>#2.2" -> "<main:rig2> twotwo",
        "<main:ag2>#2.2" -> "<ag2> agtwo agtwo")
    val portNames2Qual = Map(
        "<main.rig9:rig2>#2.2" -> "<main:rig2> twotwo",
        "<main:ag2>#2.2" -> "<main.rig9:ag2> agtwo agtwo")
    
    val setup1 = Setup().withPortNamesReplaced(portNames1)
    
    setup1.allPortNames should equal (portNames1)
    
    val setup2 = setup1.withPosUpdated(Pos("<rig9>")).withPortNames(portNames2)
    
    setup2.allPortNames should equal (portNames1 ++ portNames2Qual)
  }

  test("withSettingsReplaced - Make sure it replaces") {
    val settings1 = Map(
        "<main:rig1>#1.1" -> "oneone",
        "<main:ag1>#1.1" -> "agone agone")
    val settings2 = Map(
        "<main:rig2>#2.2" -> "twotwo",
        "<main:ag2>#2.2" -> "agtwo agtwo")
    
    val setup1 = Setup().withSettingsReplaced(settings1)
    
    setup1.allSettings should equal (settings1)
    
    val setup2 = setup1.withSettingsReplaced(settings2)
    
    setup2.allSettings should equal (settings2)
  }
  
  test("withSettingsReplaced - Defaults pos to current pos") {
    val settings1 = Map(
        "<rig1>#1.1" -> "oneone",
        "<main:ag1>#1.1" -> "agone agone")
    val setting2 = Map(
        "<main:rig2>#2.2" -> "twotwo",
        "<main:ag2>#2.2" -> "agtwo agtwo")
    
    val setup1 = Setup().withPosUpdated(Pos("<rig2>")).withSettingsReplaced(settings1)
    
    setup1.allSettings should equal (
        Map(
        "<main.rig2:rig1>#1.1" -> "oneone",
        "<main:ag1>#1.1" -> "agone agone")
    )
  }
  
  test("withSettingsRemoved - Removes settings") {
    val settings = Map(
        "<main:rig1>#1.1" -> "oneone",
        "<main:ag1>#1.1" -> "agone agone",
        "<main.rig1:ag22>#2.2" -> "twotwo",
        "<main.rig1:xx23>#2.3" -> "two three")

    val setup1 = Setup().withSettingsReplaced(settings)
    
    val test = { portID: PortID => portID.unqualified.toString startsWith "<ag" }
    val setup2 = setup1.withSettingsRemoved(test)
    
    val expectedSettings = Map(
        "<main:rig1>#1.1" -> "oneone",
        "<main.rig1:xx23>#2.3" -> "two three")
        
    setup2.allSettings should equal (expectedSettings)
  }
  
  test("withSettings - Make sure it adds settings") {
    val settings1 = Map(
        "<main:rig1>#1.1" -> "oneone",
        "<main:ag1>#1.1" -> "agone agone")
    val settings2 = Map(
        "<main:rig2>#2.2" -> "twotwo",
        "<main:ag2>#2.2" -> "agtwo agtwo")
    
    val setup1 = Setup().withSettingsReplaced(settings1)
    
    setup1.allSettings should equal (settings1)
    
    val setup2 = setup1.withSettings(settings2)
    
    setup2.allSettings should equal (settings1 ++ settings2)
  }
  
  test("withSettings - Defaults unqualified ports to current pos") {
    val settings1 = Map(
        "<main:rig1>#1.1" -> "<oneone",
        "<main:ag1>#1.1" -> "agone agone")
    val settings2 = Map(
        "<rig2>#2.2" -> "twotwo",
        "<main:ag2>#2.2" -> "agtwo agtwo")
    val settings2Qual = Map(
        "<main.rig9:rig2>#2.2" -> "twotwo",
        "<main:ag2>#2.2" -> "agtwo agtwo")
    
    val setup1 = Setup().withSettingsReplaced(settings1)
    
    setup1.allSettings should equal (settings1)
    
    val setup2 = setup1.withPosUpdated(Pos("<rig9>")).withSettings(settings2)
    
    setup2.allSettings should equal (settings1 ++ settings2Qual)
  }

  test("withConnsReplaced - Replaces all conns") {
    val connTopA = Connection("<main:rig1>#1.1", "<main:ag1>#1.1")
    val connRigA = Connection("<main.rig1:ag22>#2.2", "<main.rig1:ag23>#2.3")

    val setupA = Setup(Set(connTopA, connRigA))
    
    // Check the conns went in okay
    
    setupA.allConns should equal (Set(connTopA, connRigA))

    val connTopB = Connection("<main:rig1>#2.2", "<main:ag2>#2.3")
    val connRigB = Connection("<main.rig1:ag44>#4.4", "<main.rig1:ag45>#4.5")
    
    // Now change the conns and check them
    val setupB = setupA.withConnsReplaced(Set(connTopB, connRigB))
    
    setupB.allConns should equal (Set(connTopB, connRigB))
  }
  
  test("withConnsReplaced - Defaults unqualified port IDs to current pos") {
    val connTopA = Connection("<main:rig1>#1.1", "<main:ag1>#1.1")
    val connRigA = Connection("<main.rig1:ag22>#2.2", "<main.rig1:ag23>#2.3")

    val setupA = Setup(Set(connTopA, connRigA)).withPosUpdated(Pos("<rig1>"))
    
    // Check the conns went in okay
    
    setupA.allConns should equal (Set(connTopA, connRigA))

    val connTopB = Connection("<main:rig1>#2.2", "<ag2>#2.3")
    val connRigB = Connection("<ag44>#4.4", "<main.rig1:ag45>#4.5")
    
    // Now change the conns and check them
    val setupB = setupA.withConnsReplaced(Set(connTopB, connRigB))

    val connTopBQual = Connection("<main:rig1>#2.2", "<main.rig1:ag2>#2.3")
    val connRigBQual = Connection("<main.rig1:ag44>#4.4", "<main.rig1:ag45>#4.5")
    
    setupB.allConns should equal (Set(connTopBQual, connRigBQual))
  }
  
  test("withConns - Adds connections") {
    val conn1 = Connection("<main:rig1>#1.1", "<main:ag1>#1.1")
    val conn2 = Connection("<main:ag1>#1.1", "<main:big1>#1.1")
    
    val setup1 = Setup(Set(conn1, conn2))
    
    val conn3 = Connection("<main:big1>#1.1", "<main:cog1>#1.1")
    val conn4 = Connection("<main:cog11>#1.1", "<main:dig1>#1.1")
    
    val setup2 = setup1.withConns(Set(conn3, conn4))
    
    setup2.allConns should equal (Set(conn1, conn2, conn3, conn4))
  }
  
  test("withConns - Defaults unqualified port IDs to current pos") {
    val conn1 = Connection("<main:rig1>#1.1", "<main:ag1>#1.1")
    val conn2 = Connection("<main:ag1>#1.1", "<main:big1>#1.1")
    
    val setup1 = Setup(Set(conn1, conn2)).withPosUpdated(Pos("<rig1>"))
    
    val conn3 = Connection("<big1>#1.1", "<main:cog1>#1.1")
    val conn4 = Connection("<main:cog11>#1.1", "<dig1>#1.1")
    
    val setup2 = setup1.withConns(Set(conn3, conn4))
    
    val conn3Qual = Connection("<main.rig1:big1>#1.1", "<main:cog1>#1.1")
    val conn4Qual = Connection("<main:cog11>#1.1", "<main.rig1:dig1>#1.1")
    
    setup2.allConns should equal (Set(conn1, conn2, conn3Qual, conn4Qual))
  }
  
  test("withConns - Retains unqualified agent names correctly") {
    val conn = Connection("<rig1>#1.1", "<ag1>#1.1")

    // The unqualified port names should default to the current pos
    // which is initially the top level.
    
    val setup1 = Setup().withConns(Set(conn))
    val setup2 = setup1.withPosUpdated(Pos("<rig9>"))
    
    val expectedConn = Connection("<main:rig1>#1.1", "<main:ag1>#1.1")
    
    setup2.allConns should equal (Set(expectedConn))
  }
  
  test("withConnsRemoved - Removes specified connections") {
    val conn1 = Connection("<main:rig1>#1.1", "<main:ag1>#1.1")
    val conn2 = Connection("<main:ag1>#1.1", "<main:big1>#1.1")
    val conn3 = Connection("<main:big1>#1.1", "<main:cog1>#1.1")
    val conn4 = Connection("<main:cog11>#1.1", "<main:dig1>#1.1")
    
    val setup1 = Setup(Set(conn1, conn2, conn3, conn4))
    
    // Check the initial connections are there
    
    setup1.allConns should equal (Set(conn1, conn2, conn3, conn4))
    
    val removeMasterIgs = { c: Connection => c.master.unqualified.toString contains "ig" }
    val setup2 = setup1.withConnsRemoved(removeMasterIgs)
    
    setup2.allConns should equal (Set(conn2, conn4))
  }

  test("Equals - Simple setups") {
    Setup() should equal (Setup())

    val conn1 = Connection("<rig3> three", "<fff> five")
    
    Setup(Set(conn1)) should equal (Setup(Set(conn1)))
  }
  
  test("Equals - with a rig") {
    // One setup
    val conn1 = Connection("<rig3> three", "<fff> five")
    val rigConn1 = Connection("<main.rig3:sss>#7.7", "<main.rig3:other> other")

    val setup1WithRig = Setup(Set(conn1, rigConn1))

    // Identical setup with different vals
    val conn2 = Connection("<rig3> three", "<fff> five")
    val rigConn2 = Connection("<main.rig3:sss>#7.7", "<main.rig3:other> other")

    val setup2WithRig = Setup(Set(conn2, rigConn2))

    setup1WithRig should equal(setup2WithRig)
  }
  
  test("Equals - with pos") {
    val setupBasic1 = new Setup(Set())
    val setupBasic2 = new Setup(Set())
    val setupWithPos1 = Setup().withPosUpdated(Pos("<rig1>"))
    val setupWithPos2 = Setup().withPosUpdated(Pos("<rig1>"))
    
    setupBasic1 should equal (setupBasic2)
    setupWithPos1 should equal (setupWithPos2)
    
    setupBasic1 should not equal (setupWithPos1)
    setupWithPos1 should not equal (setupBasic1)
  }
  
  test("Equals - with portNames defaulted") {
    val setupBasic1 = new Setup(Set())
    val setupBasic2 = new Setup(Set())
    
    val portNames1 = Map("<ag1>#1.1" -> "<ag2>#2.2")
    val portNames2 = Map("<main:ag1>#1.1" -> "<main:ag2>#2.2")
    
    val setup1 = setupBasic1.withPortNames(portNames1)
    val setup2 = setupBasic2.withPortNames(portNames2)
    
    setup1 should equal (setup2)
    setup2 should equal (setup1)
  }
  
  test("Equals - with settings defaulted") {
    val setupBasic1 = new Setup(Set())
    val setupBasic2 = new Setup(Set())
    
    val settings1 = Map("<ag1>#1.1" -> "one one")
    val settings2 = Map("<main:ag1>#1.1" -> "one one")
    
    val setup1 = setupBasic1.withSettingsReplaced(settings1)
    val setup2 = setupBasic2.withSettingsReplaced(settings2)
    
    setup1 should equal (setup2)
    setup2 should equal (setup1)
  }
  
}