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
    
    setup.connsQualified should equal (Set(connTop))
  }
  
  test("conns - Just gets lower level unqualified at lower pos") {
    val connTop = Connection("<main:rig1>#1.1", "<main:ag1>#1.1")
    val connRig = Connection("<main.rig1:ag22>#2.2", "<main.rig1:ag23>#2.3")
    
    val setup = Setup(Set(connTop, connRig))
    
    setup.connsQualified(List("<rig1>")) should equal (Set(connRig))
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
    agents should contain("<a>")
    agents should contain("<b>")
    agents should contain("<c>")
  }

  test("Ports") {
    val a = "<a>#1.1"
    val b = "<b>#1.2"
    val c = "<c>#1.3"

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
    agAgConns should contain("<a>", "<b>")
    agAgConns should contain("<b>", "<c>")
    agAgConns should contain("<c>", "<b>")
  }

  test("Agent-port connections") {
    val a1 = "<a>#1.1"
    val a2 = "<a>#1.2"
    val b1 = "<b>#1.1"
    val b2 = "<b>#1.2"

    val setup = new Setup(Set(
      Connection(a1, b1),
      Connection(a2, b2)))

    val conns2 = setup.agentPortConnections

    conns2.size should equal(4)
    conns2 should contain("<a>" -> a1)
    conns2 should contain("<a>" -> a2)
    conns2 should contain("<b>" -> b1)
    conns2 should contain("<b>" -> b2)
  }

  test("Best names - Add a slave name, expect a slave updated automatically") {
    val port_a_unnamed = "<a>#1.1"
    val port_b_named = "<b> b12"
    val port_b_unnamed = "<b>#1.2"
    val port_c_unnamed = "<c>#1.3"
    
    val portMap_for_b = Map("1.2" -> "b12")

    val conn_aubu = Connection(port_a_unnamed, port_b_unnamed)

    val connSet1 = Set(conn_aubu)

    connSet1.size should equal(1)
    connSet1 should contain(conn_aubu)

    val conn_cubn = Connection(port_c_unnamed, port_b_named) // We'll add this
    val conn_aubn = Connection(port_a_unnamed, port_b_named) // This should get created

    val connSet2 = Setup(connSet1 + conn_cubn).withPortNames("<b>", portMap_for_b).connsQualified

    connSet2.size should equal(2)
    connSet2 should contain(conn_cubn.defaultQualifier(List()))
    connSet2 should not contain (conn_aubu.defaultQualifier(List()))
    connSet2 should contain(conn_aubn.defaultQualifier(List()))
  }

  test("Best names - Add a slave name, expect a master updated automatically") {
    val port_a_unnamed = "<a>#1.1"
    val port_b_named = "<b> b12"
    val port_b_unnamed = "<b>#1.2"
    val port_c_unnamed = "<c>#1.3"
    
    val portMap_for_b = Map("1.2" -> "b12")

    val conn_buau = Connection(port_b_unnamed, port_a_unnamed)

    val connSet1 = Set(conn_buau)

    connSet1.size should equal(1)
    connSet1 should contain(conn_buau)

    val conn_cubn = Connection(port_c_unnamed, port_b_named) // We'll add this
    val conn_bnau = Connection(port_b_named, port_a_unnamed) // This should get created

    val connSet2 = Setup(connSet1 + conn_cubn).withPortNames("<b>", portMap_for_b).connsQualified

    connSet2.size should equal(2)
    connSet2 should contain(conn_cubn.defaultQualifier(List()))
    connSet2 should not contain (conn_buau.defaultQualifier(List()))
    connSet2 should contain(conn_bnau.defaultQualifier(List()))
  }

  test("Best names - Add a master name, expect a slave updated automatically") {
    val port_a_unnamed = "<a>#1.1"
    val port_b_named = "<b> b12"
    val port_b_unnamed = "<b>#1.2"
    val port_c_unnamed = "<c>#1.3"
    
    val portMap_for_b = Map("1.2" -> "b12")

    val conn_aubu = Connection(port_a_unnamed, port_b_unnamed)

    val connSet1 = Set(conn_aubu)

    connSet1.size should equal(1)
    connSet1 should contain(conn_aubu)

    val conn_bncu = Connection(port_b_named, port_c_unnamed) // We'll add this
    val conn_aubn = Connection(port_a_unnamed, port_b_named) // This should get created

    val connSet2 = Setup(connSet1 + conn_bncu).withPortNames("<b>", portMap_for_b).connsQualified

    connSet2.size should equal(2)
    connSet2 should contain(conn_bncu.defaultQualifier(List()))
    connSet2 should not contain (conn_aubu.defaultQualifier(List()))
    connSet2 should contain(conn_aubn.defaultQualifier(List()))
  }

  test("Unify - Add a master name, expect a master updated automatically") {
    val port_a_unnamed = "<a>#1.1"
    val port_b_named = "<b> b12"
    val port_b_unnamed = "<b>#1.2"
    val port_c_unnamed = "<c>#1.3"
    
    val portMap_for_b = Map("1.2" -> "b12")

    val conn_buau = Connection(port_b_unnamed, port_a_unnamed)

    val connSet1 = Set(conn_buau)

    connSet1.size should equal(1)
    connSet1 should contain(conn_buau)

    val conn_bncu = Connection(port_b_named, port_c_unnamed) // We'll add this
    val conn_bnau = Connection(port_b_named, port_a_unnamed) // This should get created

    val connSet2 = Setup(connSet1 + conn_bncu).withPortNames("<b>", portMap_for_b).connsQualified

    connSet2.size should equal(2)
    connSet2 should contain(conn_bncu.defaultQualifier(List()))
    connSet2 should not contain (conn_buau.defaultQualifier(List()))
    connSet2 should contain(conn_bnau.defaultQualifier(List()))
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

    val conns2 = new Setup(conns).connsQualified

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

  test("Constructor - Make sure it unifies") {
    val port_a_unnamed = "<a>#1.1"
    val port_b_named = "<b> b12"
    val port_b_unnamed = "<b>#1.2"
    val port_c_unnamed = "<c>#1.3"
    
    val portMap_for_b = Map("1.2" -> "b12")

    val conn_buau = Connection(port_b_unnamed, port_a_unnamed)
    val conn_bncu = Connection(port_b_named, port_c_unnamed) // We'll add this
    val conn_bnau = Connection(port_b_named, port_a_unnamed) // This should get created

    val conns = Setup(Set(conn_buau, conn_bncu)).withPortNames("<b>", portMap_for_b).connsQualified

    conns.size should equal(2)
    conns should contain(conn_bncu.defaultQualifier(List()))
    conns should not contain (conn_buau.defaultQualifier(List()))
    conns should contain(conn_bnau.defaultQualifier(List()))
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
    setupV2.connsQualified should equal (Set(conn))
  }
  
  test("withConnsReplaced - No args") {
    val conn = Connection("<main:rig1> one", "<main:rig2> two")
    val setup = Setup(Set(conn))

    val rigConn1 = Connection("<main.rig1:sss>#7.7", "<main.rig1:other> other")
    val rigConn2 = Connection("<main.rig2:sss>#7.7", "<main.rig2:other> other")
    val setup1 = Setup(Set(conn, rigConn1, rigConn2))

    // Just checking some basics are right before we call the
    // method under test
    
    setup1.connsQualified should equal (Set(conn))
    setup1.rigs should equal (Set("<rig1>", "<rig2>"))
    setup1.connsQualified(List("<rig1>")) should equal (Set(rigConn1))
    
    val newConn = Connection("<main:rig1> one A", "<main:ggg> seven")
    val setupV2 = setup1.withConnsReplaced(Set(newConn))
    
    setupV2.connsQualified should equal (Set(newConn))
    setupV2.rigs should equal (Set("<rig1>", "<rig2>"))
    setupV2.connsQualified(List("<rig1>")) should equal (Set(rigConn1))
  }
  
  test("withConnsReplaced - Replacing a specific rig's conns - bottom of hierarchy") {
    val connsTop = Connection("<main:rig1> one out", "<main:top> top input")
    val connsMid = Connection("<main.rig1:rig2> two out", "<main.rig1:mid> mid input")
    val connsBottom = Connection("<main.rig1:main.rig2:lower> three out", "<main.rig1:main.rig2:bottom> bottom input")

    val setupTop = Setup(Set(connsTop, connsMid, connsBottom)).withPosUpdated(List("<rig1>", "<rig2>"))

    val connsBottom2 = Connection("<main.rig1:main.rig2:lower2> three out2", "<main.rig1:main.rig2:bottom2> bottom input2")

    val setupTop2 = setupTop.withConnsReplaced(List("<rig1>", "<rig2>"), Set(connsBottom2))
    
    setupTop2.connsQualified should equal (Set(connsTop))
    setupTop2.rigs should equal (Set("<rig1>"))
    setupTop2.pos should equal (List("<rig1>", "<rig2>"))
    
    setupTop2.connsQualified(List("<rig1>")) should equal (Set(connsMid))
    setupTop2.connsQualified(List("<rig1>", "<rig2>")) should equal (Set(connsBottom2))
  }

  test("withConnsReplaced - Replacing a specific rig's conns - middle of hierarchy") {
    val connsTop = Connection("<main:rig1> one out", "<main:top> top input")
    val connsMid = Connection("<main.rig1:rig2> two out", "<main.rig1:mid> mid input")
    val connsBottom = Connection("<main.rig1:main.rig2:lower> three out", "<main.rig1:main.rig2:bottom> bottom input")

    val setupTop = Setup(Set(connsTop, connsMid, connsBottom)).withPosUpdated(List("<rig1>", "<rig2>"))

    val connsMid2 = Connection("<main.rig1:rig2> two out2", "<main.rig1:mid> mid input2")

    val setupTop2 = setupTop.withConnsReplaced(List("<rig1>"), Set(connsMid2))
    
    setupTop2.connsQualified should equal (Set(connsTop))
    setupTop2.rigs should equal (Set("<rig1>"))
    setupTop2.pos should equal (List("<rig1>", "<rig2>"))
    
    setupTop2.connsQualified(List("<rig1>")) should equal (Set(connsMid2))
    setupTop2.connsQualified(List("<rig1>", "<rig2>")) should equal (Set(connsBottom))
  }
  
  test("withConnsReplaced - Replacing a specific rig's conns - top of hierarchy") {
    val connsTop = Connection("<main:rig1> one out", "<main:top> top input")
    val connsMid = Connection("<main.rig1:rig2> two out", "<main.rig1:mid> mid input")
    val connsBottom = Connection("<main.rig1:main.rig2:lower> three out", "<main.rig1:main.rig2:bottom> bottom input")

    val setupTop = Setup(Set(connsTop, connsMid, connsBottom)).withPosUpdated(List("<rig1>", "<rig2>"))

    val connsTop2 = Connection("<main:rig1> one out2", "<main:top> top input2")

    val setupTop2 = setupTop.withConnsReplaced(List(), Set(connsTop2))
    
    setupTop2.connsQualified should equal (Set(connsTop2))
    setupTop2.rigs should equal (Set("<rig1>"))
    setupTop2.pos should equal (List("<rig1>", "<rig2>"))
    
    setupTop2.connsQualified(List("<rig1>")) should equal (Set(connsMid))
    setupTop2.connsQualified(List("<rig1>", "<rig2>")) should equal (Set(connsBottom))
  }
  
  test("withConnsReplaced - If becomes disconnected from higher level, it should remain") {
    val connsTop = Connection("<main:rig1> one out", "<main:top> top input")
    val connsRig = Connection("<main.rig1:too> two out", "<main.rig1:mid> mid input")

    val setupTop = Setup(Set(connsTop, connsRig)).withPosUpdated(List("<rig1>"))
    
    val connsTop2 = Connection("<wig1> one out", "<top> top input")
    
    val setupTop2 = setupTop.withConnsReplaced(List(), Set(connsTop2))
    
    setupTop2.connsQualified(List("<rig1>")) should equal (Set(connsRig))
  }
  
  test("withConnsReplaced - If rig appears in connections, should be able to ask for its connections") {
    val connsTop = Connection("<main:rig1> one out", "<main:top> top input")
    val connsRig = Connection("<main.rig1:too> two out", "<main.rig1:mid> mid input")
    
    val setupTop = Setup(Set(connsTop, connsRig)).withPosUpdated(List("<rig1>"))

    val connsTopPlus = Connection("<any> any out", "<rig3> three input")
    
    val setupTopPlus = setupTop.withConnsReplaced(List(), Set(connsTop, connsTopPlus))
    
    setupTopPlus.rigs should equal (Set("<rig1>", "<rig3>"))
    setupTopPlus.connsQualified(List("<rig1>")) should equal (Set(connsRig))
    setupTopPlus.connsQualified(List("<rig3>")) should equal (Set())
  }
  
  test("withConnsReplaced - If rig remains in connections, its connections should remain") {
    val connsTop = Connection("<main:rig1> one out", "<main:top> top input")
    val connsRig = Connection("<main.rig1:too> two out", "<main.rig1:mid> mid input")

    val setupTop = Setup(Set(connsTop, connsRig)).withPosUpdated(List("<rig1>"))
    
    // <rig1> is still part of this new set of connections
    val connsTop2 = Connection("<main:back1> back out", "<main:rig1> one input")
    
    val setupTop2 = setupTop.withConnsReplaced(List(), Set(connsTop2))
    
    setupTop2.connsQualified should equal (Set(connsTop2))
    setupTop2.connsQualified(List("<rig1>")) should equal (Set(connsRig))
  }

}