package org.pigsaw.eigendmapper

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers

import org.pigsaw.eigendmapper.Graphable._

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
  
  test("Position - Can update position") {
    val conn = Connection("<main:rig1> one", "<main:fff> five")
    val setup = new Setup(Set(conn))
    
    setup.pos should equal (List())
    
    val setupV2 = setup.withPosUpdated(List("<rig1>"))
    
    setupV2.pos should equal (List("<rig1>"))
    setupV2.conns should equal (Set(conn))
  }
  
  test("withConnsReplaced - No args") {
    val conn = Connection("<main:rig1> one", "<main:rig2> two")
    val setup = Setup(Set(conn))

    val rigConn1 = Connection("<main.rig1:sss>#7.7", "<main.rig1:other> other")
    val rigConn2 = Connection("<main.rig2:sss>#7.7", "<main.rig2:other> other")
    val setup1 = Setup(Set(conn, rigConn1, rigConn2))

    // Just checking some basics are right before we call the
    // method under test
    
    setup1.conns should equal (Set(conn))
    setup1.rigs should equal (Set("<rig1>", "<rig2>"))
    setup1.conns(List("<rig1>")) should equal (Set(rigConn1))
    
    val newConn = Connection("<main:rig1> one A", "<main:ggg> seven")
    val setupV2 = setup1.withConnsReplaced(Set(newConn))
    
    setupV2.conns should equal (Set(newConn))
    setupV2.rigs should equal (Set("<rig1>", "<rig2>"))
    setupV2.conns(List("<rig1>")) should equal (Set(rigConn1))
  }
  
  test("withConnsReplaced - Replacing a specific rig's conns - bottom of hierarchy") {
    val connsTop = Connection("<main:rig1> one out", "<main:top> top input")
    val connsMid = Connection("<main.rig1:rig2> two out", "<main.rig1:mid> mid input")
    val connsBottom = Connection("<main.rig1:main.rig2:lower> three out", "<main.rig1:main.rig2:bottom> bottom input")

    val setupTop = Setup(Set(connsTop, connsMid, connsBottom)).withPosUpdated(List("<rig1>", "<rig2>"))

    val connsBottom2 = Connection("<main.rig1:main.rig2:lower2> three out2", "<main.rig1:main.rig2:bottom2> bottom input2")

    val setupTop2 = setupTop.withConnsReplaced(List("<rig1>", "<rig2>"), Set(connsBottom2))
    
    setupTop2.conns should equal (Set(connsTop))
    setupTop2.rigs should equal (Set("<rig1>"))
    setupTop2.pos should equal (List("<rig1>", "<rig2>"))
    
    setupTop2.conns(List("<rig1>")) should equal (Set(connsMid))
    setupTop2.conns(List("<rig1>", "<rig2>")) should equal (Set(connsBottom2))
  }

  test("withConnsReplaced - Replacing a specific rig's conns - middle of hierarchy") {
    val connsTop = Connection("<main:rig1> one out", "<main:top> top input")
    val connsMid = Connection("<main.rig1:rig2> two out", "<main.rig1:mid> mid input")
    val connsBottom = Connection("<main.rig1:main.rig2:lower> three out", "<main.rig1:main.rig2:bottom> bottom input")

    val setupTop = Setup(Set(connsTop, connsMid, connsBottom)).withPosUpdated(List("<rig1>", "<rig2>"))

    val connsMid2 = Connection("<main.rig1:rig2> two out2", "<main.rig1:mid> mid input2")

    val setupTop2 = setupTop.withConnsReplaced(List("<rig1>"), Set(connsMid2))
    
    setupTop2.conns should equal (Set(connsTop))
    setupTop2.rigs should equal (Set("<rig1>"))
    setupTop2.pos should equal (List("<rig1>", "<rig2>"))
    
    setupTop2.conns(List("<rig1>")) should equal (Set(connsMid2))
    setupTop2.conns(List("<rig1>", "<rig2>")) should equal (Set(connsBottom))
  }
  
  test("withConnsReplaced - Replacing a specific rig's conns - top of hierarchy") {
    val connsTop = Connection("<main:rig1> one out", "<main:top> top input")
    val connsMid = Connection("<main.rig1:rig2> two out", "<main.rig1:mid> mid input")
    val connsBottom = Connection("<main.rig1:main.rig2:lower> three out", "<main.rig1:main.rig2:bottom> bottom input")

    val setupTop = Setup(Set(connsTop, connsMid, connsBottom)).withPosUpdated(List("<rig1>", "<rig2>"))

    val connsTop2 = Connection("<main:rig1> one out2", "<main:top> top input2")

    val setupTop2 = setupTop.withConnsReplaced(List(), Set(connsTop2))
    
    setupTop2.conns should equal (Set(connsTop2))
    setupTop2.rigs should equal (Set("<rig1>"))
    setupTop2.pos should equal (List("<rig1>", "<rig2>"))
    
    setupTop2.conns(List("<rig1>")) should equal (Set(connsMid))
    setupTop2.conns(List("<rig1>", "<rig2>")) should equal (Set(connsBottom))
  }
  
  test("withConnsReplaced - If becomes disconnected from higher level, it should remain") {
    val connsTop = Connection("<main:rig1> one out", "<main:top> top input")
    val connsRig = Connection("<main.rig1:too> two out", "<main.rig1:mid> mid input")

    val setupTop = Setup(Set(connsTop, connsRig)).withPosUpdated(List("<rig1>"))
    
    val connsTop2 = Connection("<wig1> one out", "<top> top input")
    
    val setupTop2 = setupTop.withConnsReplaced(List(), Set(connsTop2))
    
    setupTop2.conns(List("<rig1>")) should equal (Set(connsRig))
  }
  
  test("withConnsReplaced - If rig appears in connections, should be able to ask for its connections") {
    val connsTop = Connection("<main:rig1> one out", "<main:top> top input")
    val connsRig = Connection("<main.rig1:too> two out", "<main.rig1:mid> mid input")
    
    val setupTop = Setup(Set(connsTop, connsRig)).withPosUpdated(List("<rig1>"))

    val connsTopPlus = Connection("<any> any out", "<rig3> three input")
    
    val setupTopPlus = setupTop.withConnsReplaced(List(), Set(connsTop, connsTopPlus))
    
    setupTopPlus.rigs should equal (Set("<rig1>", "<rig3>"))
    setupTopPlus.conns(List("<rig1>")) should equal (Set(connsRig))
    setupTopPlus.conns(List("<rig3>")) should equal (Set())
  }
  
  test("withConnsReplaced - If rig remains in connections, its connections should remain") {
    val connsTop = Connection("<main:rig1> one out", "<main:top> top input")
    val connsRig = Connection("<main.rig1:too> two out", "<main.rig1:mid> mid input")

    val setupTop = Setup(Set(connsTop, connsRig)).withPosUpdated(List("<rig1>"))
    
    // <rig1> is still part of this new set of connections
    val connsTop2 = Connection("<main:back1> back out", "<main:rig1> one input")
    
    val setupTop2 = setupTop.withConnsReplaced(List(), Set(connsTop2))
    
    setupTop2.conns should equal (Set(connsTop2))
    setupTop2.conns(List("<rig1>")) should equal (Set(connsRig))
  }

}