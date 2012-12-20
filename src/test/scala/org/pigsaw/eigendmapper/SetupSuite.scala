package org.pigsaw.eigendmapper

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers

import org.pigsaw.eigendmapper.Graphable._

@RunWith(classOf[JUnitRunner])
class SetupSuite extends FunSuite with ShouldMatchers {
  
  test("conns - Just gets top level if at top level") {
    val connTop = Connection("<rig1>#1.1", "<ag1>#1.1")
    val connRig = Connection("<main.rig1:ag22>#2.2", "<main.rig1:ag23>#2.3")
    
    val setup = Setup(Set(connTop, connRig))
    
    setup.conns should equal (Set(connTop))
  }
  
  test("conns - Just gets lower level unqualified at lower pos") {
    val connTop = Connection("<rig1>#1.1", "<ag1>#1.1")
    val connRig = Connection("<main.rig1:ag22>#2.2", "<main.rig1:ag23>#2.3")
    val connRigUnqual = Connection("<ag22>#2.2", "<ag23>#2.3")
    
    val setup = Setup(Set(connTop, connRig))
    
    setup.conns(List("<rig1>")) should equal (Set(connRigUnqual))
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

  /*test("Agent-port connections - with unparseable agent name") {
    val a1 = DeprecatedPort("<a>#1.1", None)
    val a2 = DeprecatedPort("a#1.2", Some("b12"))
    val b1 = DeprecatedPort("b#1.1", None)
    val b2 = DeprecatedPort("b#1.2", None)

    val setup = new Setup(Set(
      Connection(a1, b1),
      Connection(a2, b2)))

    val conns2 = setup.agentPortConnections

    conns2.size should equal(4)
    conns2 should contain("<a>" -> a1)
    conns2 should contain("UNKNOWN" -> a2)
    conns2 should contain("UNKNOWN" -> b1)
    conns2 should contain("UNKNOWN" -> b2)
  }*/

  test("Unify - Add a slave name, expect a slave updated automatically") {
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

    val connSet2 = Setup(connSet1 + conn_cubn).withPortNames("<b>", portMap_for_b).conns

    connSet2.size should equal(2)
    connSet2 should contain(conn_cubn)
    connSet2 should not contain (conn_aubu)
    connSet2 should contain(conn_aubn)
  }

  test("Unify - Add a slave name, expect a master updated automatically") {
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

    val connSet2 = Setup(connSet1 + conn_cubn).withPortNames("<b>", portMap_for_b).conns

    connSet2.size should equal(2)
    connSet2 should contain(conn_cubn)
    connSet2 should not contain (conn_buau)
    connSet2 should contain(conn_bnau)
  }

  test("Unify - Add a master name, expect a slave updated automatically") {
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

    val connSet2 = Setup(connSet1 + conn_bncu).withPortNames("<b>", portMap_for_b).conns

    connSet2.size should equal(2)
    connSet2 should contain(conn_bncu)
    connSet2 should not contain (conn_aubu)
    connSet2 should contain(conn_aubn)
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

    val connSet2 = Setup(connSet1 + conn_bncu).withPortNames("<b>", portMap_for_b).conns

    connSet2.size should equal(2)
    connSet2 should contain(conn_bncu)
    connSet2 should not contain (conn_buau)
    connSet2 should contain(conn_bnau)
  }

  test("Unqualified port IDs automatically (cut the 'main:' in <main:agentname3>)") {
    val a_short = "<a>#1.1"
    val a_long = "<main:a>#1.1"
    val b_short = "<b> b12"
    val b_long = "<main:b> b12"
    val c_short = "<c>#1.3"
    val c_long = "<main:c>#1.3"

    val conns = Set(
      Connection(a_short, b_long),
      Connection(b_long, c_long),
      Connection(b_short, c_long))

    val conns2 = new Setup(conns).conns

    conns2.size should equal(2)
    conns2 should not contain (Connection(a_short, b_long))
    conns2 should not contain (Connection(b_long, c_long))
    conns2 should not contain (Connection(b_short, c_long))
    conns2 should contain(Connection(a_short, b_short))
    conns2 should contain(Connection(b_short, c_short))

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

  test("Apply - Make sure it unqualifies") {
    val a_short = "<a>#1.1"
    val a_long = "<main:a>#1.1"
    val b_short = "<b> b12"
    val b_long = "<main:b> b12"
    val c_short = "<c>#1.3"
    val c_long = "<main:c>#1.3"

    val conns = Set(
      Connection(a_short, b_long),
      Connection(b_long, c_long),
      Connection(b_short, c_long))

    val conns2 = Setup(conns).conns

    conns2.size should equal(2)
    conns2 should not contain (Connection(a_short, b_long))
    conns2 should not contain (Connection(b_long, c_long))
    conns2 should not contain (Connection(b_short, c_long))
    conns2 should contain(Connection(a_short, b_short))
    conns2 should contain(Connection(b_short, c_short))

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

    val conns = Setup(Set(conn_buau, conn_bncu)).withPortNames("<b>", portMap_for_b).conns

    conns.size should equal(2)
    conns should contain(conn_bncu)
    conns should not contain (conn_buau)
    conns should contain(conn_bnau)
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
  
  test("Position - Can update position") {
    val conn = Connection("<rig1> one", "<fff> five")
    val setup = new Setup(Set(conn))
    
    setup.pos should equal (List())
    
    val setupV2 = setup.withPosUpdated(List("<rig1>"))
    
    setupV2.pos should equal (List("<rig1>"))
    setupV2.conns should equal (Set(conn))
  }
  
  test("withConnsReplaced - No args") {
    val conn = Connection("<rig1> one", "<rig2> two")
    val setup = Setup(Set(conn))

    val rigConn1 = Connection("<main.rig1:sss>#7.7", "<main.rig1:other> other")
    val rigConn2 = Connection("<main.rig2:sss>#7.7", "<main.rig2:other> other")
    val setup1 = Setup(Set(conn, rigConn1, rigConn2))

    // Just checking some basics are right before we call the
    // method under test
    
    setup1.conns should equal (Set(conn))
    setup1.rigs should equal (Set("<rig1>"))
    setup1.conns(List("<rig1>")) should equal (rigConn1)
    
    val newConn = Connection("<rig1> one A", "<ggg> seven")
    val setupV2 = setup1.withConnsReplaced(Set(newConn))
    
    setupV2.conns should equal (Set(newConn))
    setupV2.rigs should equal (Set("<rig1>"))
    setupV2.conns(List("<rig1>")) should equal (rigConn1)
  }
  
  test("withConnsReplaced - Replacing a specific rig's conns - bottom of hierarchy") {
    val connsTop = Connection("<rig1> one out", "<top> top input")
    val connsMid = Connection("<main.rig1:rig2> two out", "<main.rig1:mid> mid input")
    val connsBottom = Connection("<main.rig1:main.rig2:lower> three out", "<main.rig1:main.rig2:bottom> bottom input")

    val connsMidUnqual = Connection("<rig2> two out", "<mid> mid input")

    val setupTop = Setup(Set(connsTop, connsMid, connsBottom)).withPosUpdated(List("<rig1>", "<rig2>"))

    val connsBottom2 = Connection("<main.rig1:main.rig2:lower2> three out2", "<main.rig1:main.rig2:bottom2> bottom input2")
    val connsBottom2Unqual = Connection("<lower2> three out2", "<bottom2> bottom input2")

    val setupTop2 = setupTop.withConnsReplaced(List("<rig1>", "<rig2>"), Set(connsBottom2))
    
    setupTop2.conns should equal (Set(connsTop))
    setupTop2.rigs should equal (Set("<rig1>"))
    setupTop2.pos should equal (List("<rig1>", "<rig2>"))
    
    setupTop2.conns(List("<rig1>")) should equal (Set(connsMidUnqual))
    setupTop2.conns(List("<rig1>", "<rig2>")) should equal (Set(connsBottom2Unqual))
  }

  test("withConnsReplaced - Replacing a specific rig's conns - middle of hierarchy") {
    val connsTop = Connection("<rig1> one out", "<top> top input")
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
    val connsTop = Connection("<rig1> one out", "<top> top input")
    val connsMid = Connection("<main.rig1:rig2> two out", "<main.rig1:mid> mid input")
    val connsBottom = Connection("<main.rig1:main.rig2:lower> three out", "<main.rig1:main.rig2:bottom> bottom input")
    
    val setupTop = Setup(Set(connsTop, connsMid, connsBottom)).withPosUpdated(List("<rig1>", "<rig2>"))

    val connsTop2 = Connection("<rig1> one out2", "<top> top input2")

    val setupTop2 = setupTop.withConnsReplaced(List(), Set(connsTop2))
    
    setupTop2.conns should equal (Set(connsTop2))
    setupTop2.rigs should equal (Set("<rig1>"))
    setupTop2.pos should equal (List("<rig1>", "<rig2>"))
    
    setupTop2.conns(List("<rig1>")) should equal (Set(connsMid))
    setupTop2.conns(List("<rig1>", "<rig2>")) should equal (Set(connsBottom))
  }
  
  test("withConnsReplaced - If rig disappears from connections, should disappear from rigSetups") {
    val connsTop = Connection("<rig1> one out", "<top> top input")
    val connsRig = Connection("<main.rig1:too> two out", "<main.rig1:mid> mid input")
    
    val setupTop = Setup(Set(connsTop, connsRig)).withPosUpdated(List("<rig1>"))
    
    val connsTop2 = Connection("<wig1> one out", "<top> top input")
    
    val setupTop2 = setupTop.withConnsReplaced(List(), Set(connsTop2))
    
    setupTop2.conns(List("<rig1>")) should equal (Set())
  }
  
  test("withConnsReplaced - If rig appears in connections, should appear in rigSetups") {
    val connsTop = Connection("<rig1> one out", "<top> top input")
    val connsRig = Connection("<main.rig1:too> two out", "<main.rig1:mid> mid input")
    
    val setupTop = Setup(Set(connsTop, connsRig)).withPosUpdated(List("<rig1>"))

    val connsTopPlus = Connection("<any> any out", "<rig3> three input")
    
    val setupTopPlus = setupTop.withConnsReplaced(List(), Set(connsTop, connsTopPlus))
    
    setupTopPlus.rigs should equal (Set("<rig1>", "<rig3>"))
    setupTopPlus.conns(List("<rig1>")) should equal (Set(connsRig))
    setupTopPlus.conns(List("<rig3>")) should equal (Set())
  }
  
  test("withConnsReplaced - If rig remains in connections, should remain in rigSetups") {
    val connsTop = Connection("<rig1> one out", "<top> top input")
    val connsRig = Connection("<main.rig1:too> two out", "<main.rig1:mid> mid input")
    
    val setupTop = Setup(Set(connsTop, connsRig)).withPosUpdated(List("<rig1>"))
    
    // <rig1> is still part of this new set of connections
    val connsTop2 = Connection("<back1> back out", "<rig1> one input")
    
    val setupTop2 = setupTop.withConnsReplaced(List(), Set(connsTop2))
    
    setupTop2.conns should equal (Set(connsTop2))
    setupTop2.conns(List("<rig1>")) should equal (Set(connsRig))
  }

}