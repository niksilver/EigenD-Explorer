package org.pigsaw.eigendmapper

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers

import org.pigsaw.eigendmapper.Preamble._

@RunWith(classOf[JUnitRunner])
class SetupSuite extends FunSuite with ShouldMatchers {
  
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
    
    setup.conns(List("<rig1>")) should equal (Set(connRig))
  }
  
  test("conns - Without an argument gives connections at pos") {
    val connTop = Connection("<main:rig1>#1.1", "<main:ag1>#1.1")
    val connRig = Connection("<main.rig1:ag22>#2.2", "<main.rig1:ag23>#2.3")
    
    val setup1 = Setup(Set(connTop, connRig)).withPosUpdated(List())
    setup1.conns should equal (Set(connTop))
    
    val setup2 = setup1.withPosUpdated(List("<rig1>"))
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
    
    setup.conns(List()) should equal (Set(connTopQual))
    setup.conns(List("<rig1>")) should equal (Set(connRig))
  }
  
  test("allConns - Gets all connections, fully qualified") {
    val connTop = Connection("<rig1>#1.1", "<ag1>#1.1")
    val connRig = Connection("<main.rig1:ag22>#2.2", "<main.rig1:ag23>#2.3")

    val connTopQual = Connection("<main:rig1>#1.1", "<main:ag1>#1.1")

    val setup = Setup(Set(connTop, connRig))
    
    setup.allConns should equal (Set(connTopQual, connRig))
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
    
    val setup1 = Setup().withPosUpdated(List("<rig2>")).withPortNamesReplaced(portNames1)
    
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
    
    val test = { portID: String => portID.unqualified startsWith "<ag" }
    val setup2 = setup1.withPortNamesRemoved(test)
    
    val expectedPortNames = Map(
        "<main:rig1>#1.1" -> "<main:rig1> oneone",
        "<main.rig1:xx23>#2.3" -> "<main.rig1:xx23> two three")
        
    setup2.allPortNames should equal (expectedPortNames)
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
    
    val setup2 = setup1.withPosUpdated(List("<rig9>")).withPortNames(portNames2)
    
    setup2.allPortNames should equal (portNames1 ++ portNames2Qual)
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
    agents should contain("<main:a>")
    agents should contain("<main:b>")
    agents should contain("<main:c>")
  }

  test("Ports") {
    val a = "<main:a>#1.1"
    val b = "<main:b>#1.2"
    val c = "<main:c>#1.3"

    val setup = new Setup(Set(
      Connection(a, b),
      Connection(b, c)))

    val ports = setup.ports

    ports.size should equal(3)
    ports should contain(a)
    ports should contain(b)
    ports should contain(c)
  }

  test("Agent-agent connections") {
    val a = "<a>#1.1"
    val b = "<b>#1.2"
    val c1 = "<c>#1.3"
    val c2 = "<c>#2.3"

    val setup = new Setup(Set(
      Connection(a, b),
      Connection(b, c1),
      Connection(c1, b),
      Connection(c2, b)))

    val agAgConns = setup.agentAgentConnections

    agAgConns.size should equal(3)
    agAgConns should contain("<main:a>", "<main:b>")
    agAgConns should contain("<main:b>", "<main:c>")
    agAgConns should contain("<main:c>", "<main:b>")
  }

  test("Agent-port connections") {
    val a1 = "<main:a>#1.1"
    val a2 = "<main:a>#1.2"
    val b1 = "<main:b>#1.1"
    val b2 = "<main:b>#1.2"

    val setup = new Setup(Set(
      Connection(a1, b1),
      Connection(a2, b2)))

    val conns2 = setup.agentPortConnections

    conns2.size should equal(4)
    conns2 should contain("<main:a>" -> a1)
    conns2 should contain("<main:a>" -> a2)
    conns2 should contain("<main:b>" -> b1)
    conns2 should contain("<main:b>" -> b2)
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
    val setupWithPos1 = Setup().withPosUpdated(List("<rig1>"))
    val setupWithPos2 = Setup().withPosUpdated(List("<rig1>"))
    
    setupBasic1 should equal (setupBasic2)
    setupWithPos1 should equal (setupWithPos2)
    
    setupBasic1 should not equal (setupWithPos1)
    setupWithPos1 should not equal (setupBasic1)
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
    setup.rigs should contain("<rig3>")
    setup.rigs should contain("<rig5>")
    setup.rigs should contain("<rig7>")
  }

  test("Rigs - Detects disconnected rigs") {
    val conn1 = Connection("<ag3> three", "<ag5> five")
    val connRigA = Connection("<main.rig1:ag7>#7.7", "<main.rig1:other> other")
    val connRigB = Connection("<main.rig1:main.rig2:ag9>#9.9", "<main.rig1:main.rig2:other> other")
    val setup = Setup(Set(conn1, connRigA, connRigB))

    setup.rigs should equal (Set("<rig1>"))
    setup.rigs(List("<rig1>")) should equal (Set("<rig2>"))
  }
  
  test("Rigs - Defaults to current pos") {
    val connTop = Connection("<main:rig1>#1.1", "<main:ag1>#1.1")
    val connRig = Connection("<main.rig1:rig2>#2.2", "<main.rig1:ag23>#2.3")
    
    val setup1 = Setup(Set(connTop, connRig)).withPosUpdated(List())
    setup1.rigs should equal (Set("<rig1>"))
    
    val setup2 = setup1.withPosUpdated(List("<rig1>"))
    setup2.rigs should equal (Set("<rig2>"))
  }
  
  test("Position - Can update position") {
    val conn = Connection("<main:rig1> one", "<main:fff> five")
    val setup = new Setup(Set(conn))
    
    setup.pos should equal (List())
    
    val setupV2 = setup.withPosUpdated(List("<rig1>"))
    
    setupV2.pos should equal (List("<rig1>"))
    setupV2.allConns should equal (Set(conn))
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

    val setupA = Setup(Set(connTopA, connRigA)).withPosUpdated(List("<rig1>"))
    
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
    
    val setup1 = Setup(Set(conn1, conn2)).withPosUpdated(List("<rig1>"))
    
    val conn3 = Connection("<big1>#1.1", "<main:cog1>#1.1")
    val conn4 = Connection("<main:cog11>#1.1", "<dig1>#1.1")
    
    val setup2 = setup1.withConns(Set(conn3, conn4))
    
    val conn3Qual = Connection("<main.rig1:big1>#1.1", "<main:cog1>#1.1")
    val conn4Qual = Connection("<main:cog11>#1.1", "<main.rig1:dig1>#1.1")
    
    setup2.allConns should equal (Set(conn1, conn2, conn3Qual, conn4Qual))
  }
  
  test("withConnsRemoved - Removes specified connections") {
    val conn1 = Connection("<main:rig1>#1.1", "<main:ag1>#1.1")
    val conn2 = Connection("<main:ag1>#1.1", "<main:big1>#1.1")
    val conn3 = Connection("<main:big1>#1.1", "<main:cog1>#1.1")
    val conn4 = Connection("<main:cog11>#1.1", "<main:dig1>#1.1")
    
    val setup1 = Setup(Set(conn1, conn2, conn3, conn4))
    
    // Check the initial connections are there
    
    setup1.allConns should equal (Set(conn1, conn2, conn3, conn4))
    
    val removeMasterIgs = { c: Connection => c.master.unqualified contains "ig" }
    val setup2 = setup1.withConnsRemoved(removeMasterIgs)
    
    setup2.allConns should equal (Set(conn2, conn4))
  }
  
}