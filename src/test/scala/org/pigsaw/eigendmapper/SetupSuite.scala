package org.pigsaw.eigendmapper

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers

import org.pigsaw.eigendmapper.Graphable._

@RunWith(classOf[JUnitRunner])
class SetupSuite extends FunSuite with ShouldMatchers {

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
    val setup1 = Setup(Set(conn1))

    val rigConn1 = Connection("<sss>#7.7", "<other> other")
    val rigSetup1 = Setup(Set(rigConn1))

    val setup1WithRig = setup1.withRig("<rig3>", rigSetup1)

    // Identical setup with different vals
    val conn2 = Connection("<rig3> three", "<fff> five")
    val setup2 = Setup(Set(conn2))

    val rigConn2 = Connection("<sss>#7.7", "<other> other")
    val rigSetup2 = Setup(Set(rigConn2))

    val setup2WithRig = setup2.withRig("<rig3>", rigSetup2)

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

  test("Rigs - Contains rig setups") {
    val setup = Setup()
    val rigSetups: Map[String, Setup] = setup.rigSetups

    rigSetups.size should equal(0)
  }

  test("Rigs - Can add one rig") {
    val conn = Connection("<rig1> three", "<fff> five")
    val setup = Setup(Set(conn))

    val rigConn = Connection("<sss>#7.7", "<other> other")
    val rigSetup = Setup(Set(rigConn))

    val setup2 = setup.withRig("<rig1>", rigSetup)

    setup2.rigSetups.size should equal(1)
    setup2.rigSetups should contain("<rig1>" -> rigSetup)
  }

  test("Rigs - Can add two rigs") {
    val connA = Connection("<rig1> three", "<fff> five")
    val connB = Connection("<fff> five", "<rig2> two")
    val setup = Setup(Set(connA, connB))

    val rigConn1 = Connection("<sss>#7.7", "<other> other")
    val rigSetup1 = Setup(Set(rigConn1))

    val rigConn2 = Connection("<eee>#8.8", "<other> other")
    val rigSetup2 = Setup(Set(rigConn2))

    val setup2 = setup.withRig("<rig1>", rigSetup1).withRig("<rig2>", rigSetup2)

    setup2.rigSetups.size should equal(2)
    setup2.rigSetups should contain("<rig1>" -> rigSetup1)
    setup2.rigSetups should contain("<rig2>" -> rigSetup2)
  }
  
  test("Rigs - Adding rig preserves position") {
    val conn = Connection("<rig1> one", "<fff> five")
    val setup = Setup(Set(conn)).withPosUpdated(List("<rigX>"))

    val rigConn = Connection("<sss>#7.7", "<other> other")
    val rigSetup = Setup(Set(rigConn))

    // This should be uncontroversial
    setup.pos should equal (List("<rigX>"))
    
    val setupV2 = setup.withRig("<rig1>", rigSetup)

    // This should not have changed in the revised setup
    setupV2.pos should equal (List("<rigX>"))
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
    val conn = Connection("<rig1> one", "<fff> five")
    val setup = Setup(Set(conn))

    val rigConn1 = Connection("<sss>#7.7", "<other> other")
    val rigSetup1 = Setup(Set(rigConn1))

    val setupV2 = setup.withRig("<rig1>", rigSetup1).withRig("<rig2>", rigSetup1)
    
    // Just checking some basics are right before we call the
    // method under test
    
    setupV2.conns should equal (Set(conn))
    setupV2.rigs should equal (Set("<rig1>"))
    setupV2.rigSetups("<rig1>") should equal (rigSetup1)
    
    val newConn = Connection("<rig1> one", "<ggg> seven")
    val setupV3 = setupV2.withConnsReplaced(Set(newConn))
    
    setupV3.conns should equal (Set(newConn))
    setupV3.rigs should equal (Set("<rig1>"))
    setupV3.rigSetups("<rig1>") should equal (rigSetup1)
  }
  
  test("withConnsReplaced - Replacing a specific rig's conns - bottom of hierarchy") {
    val connsTop = Connection("<rig1> one out", "<top> top input")
    val connsMid = Connection("<rig2> two out", "<mid> mid input")
    val connsBottom = Connection("<lower> three out", "<bottom> bottom input")
    
    val setupBottom = new Setup(Set(connsBottom))
    val setupMid = Setup(Set(connsMid)).withRig("<rig2>", setupBottom)
    val setupTop = Setup(Set(connsTop)).withRig("<rig1>", setupMid).withPosUpdated(List("<rig1>", "<rig2>"))

    val connsBottom2 = Connection("<lower2> three out2", "<bottom2> bottom input2")

    val setupTop2 = setupTop.withConnsReplaced(List("<rig1>", "<rig2>"), Set(connsBottom2))
    
    setupTop2.conns should equal (Set(connsTop))
    setupTop2.rigSetups.keySet should equal (Set("<rig1>"))
    setupTop2.pos should equal (List("<rig1>", "<rig2>"))
    
    setupTop2.rigSetups("<rig1>").conns should equal (Set(connsMid))
    setupTop2.rigSetups("<rig1>").rigSetups.keys should equal (Set("<rig2>"))
    
    setupTop2.rigSetups("<rig1>").rigSetups("<rig2>").conns should equal (Set(connsBottom2))
  }

  test("withConnsReplaced - Replacing a specific rig's conns - middle of hierarchy") {
    val connsTop = Connection("<rig1> one out", "<top> top input")
    val connsMid = Connection("<rig2>#2.2 two out", "<mid> mid input")
    val connsBottom = Connection("<lower> three out", "<bottom> bottom input")
    
    val setupBottom = new Setup(Set(connsBottom))
    val setupMid = Setup(Set(connsMid)).withRig("<rig2>", setupBottom)
    val setupTop = Setup(Set(connsTop)).withRig("<rig1>", setupMid).withPosUpdated(List("<rig1>", "<rig2>"))

    val connsMid2 = Connection("<rig2> two out2", "<mid> mid input2")

    val setupTop2 = setupTop.withConnsReplaced(List("<rig1>"), Set(connsMid2))
    
    setupTop2.conns should equal (Set(connsTop))
    setupTop2.rigSetups.keySet should equal (Set("<rig1>"))
    setupTop2.pos should equal (List("<rig1>", "<rig2>"))
    
    setupTop2.rigSetups("<rig1>").conns should equal (Set(connsMid2))
    setupTop2.rigSetups("<rig1>").rigSetups.keys should equal (Set("<rig2>"))
    
    setupTop2.rigSetups("<rig1>").rigSetups("<rig2>").conns should equal (Set(connsBottom))
  }
  
  test("withConnsReplaced - Replacing a specific rig's conns - top of hierarchy") {
    val connsTop = Connection("<rig1> one out", "<top> top input")
    val connsMid = Connection("<rig2> two out", "<mid> mid input")
    val connsBottom = Connection("<lower> three out", "<bottom> bottom input")
    
    val setupBottom = new Setup(Set(connsBottom))
    val setupMid = Setup(Set(connsMid)).withRig("<rig2>", setupBottom)
    val setupTop = Setup(Set(connsTop)).withRig("<rig1>", setupMid).withPosUpdated(List("<rig1>", "<rig2>"))

    val connsTop2 = Connection("<rig1> one out2", "<top> top input2")

    val setupTop2 = setupTop.withConnsReplaced(List(), Set(connsTop2))
    
    setupTop2.conns should equal (Set(connsTop2))
    setupTop2.rigSetups.keySet should equal (Set("<rig1>"))
    setupTop2.pos should equal (List("<rig1>", "<rig2>"))
    
    setupTop2.rigSetups("<rig1>").conns should equal (Set(connsMid))
    setupTop2.rigSetups("<rig1>").rigSetups.keys should equal (Set("<rig2>"))
    
    setupTop2.rigSetups("<rig1>").rigSetups("<rig2>").conns should equal (Set(connsBottom))
  }
  
  test("withConnsReplaced - If rig disappears from connections, should disappear from rigSetups") {
    val connsTop = Connection("<rig1> one out", "<top> top input")
    val connsRig = Connection("<too> two out", "<mid> mid input")
    
    val setupRig = Setup(Set(connsRig))
    val setupTop = Setup(Set(connsTop)).withRig("<rig1>", setupRig).withPosUpdated(List("<rig1>"))
    
    val connsTop2 = Connection("<wig1> one out", "<top> top input")
    
    val setupTop2 = setupTop.withConnsReplaced(List(), Set(connsTop2))
    
    setupTop2.rigSetups.size should equal (0)
  }
  
  test("withConnsReplaced - If rig appears in connections, should appear in rigSetups") {
    val connsTop = Connection("<rig1> one out", "<top> top input")
    val connsRig = Connection("<too> two out", "<mid> mid input")
    
    val setupRig = Setup(Set(connsRig))
    val setupTop = Setup(Set(connsTop)).withRig("<rig1>", setupRig).withPosUpdated(List("<rig1>"))

    val connsTopPlus = Connection("<any> any out", "<rig3> three input")
    
    val setupTopPlus = setupTop.withConnsReplaced(List(), Set(connsTop, connsTopPlus))
    
    setupTopPlus.rigSetups.keys should equal (Set("<rig1>", "<rig3>"))
    setupTopPlus.rigSetups("<rig1>") should equal (setupRig)
    setupTopPlus.rigSetups("<rig3>") should equal (Setup())
  }
  
  test("withConnsReplaced - If rig remains in connections, should remain in rigSetups") {
    val connsTop = Connection("<rig1> one out", "<top> top input")
    val connsRig = Connection("<too> two out", "<mid> mid input")
    
    val setupRig = Setup(Set(connsRig))
    val setupTop = Setup(Set(connsTop)).withRig("<rig1>", setupRig).withPosUpdated(List("<rig1>"))
    
    // <rig1> is still part of this new set of connections
    val connsTop2 = Connection("<back1> back out", "<rig1> one input")
    
    val setupTop2 = setupTop.withConnsReplaced(List(), Set(connsTop2))
    
    setupTop2.rigSetups.size should equal (1)
    setupTop2.rigSetups("<rig1>") should equal (setupRig)
  }

  test("setupForPos") {
    val connsTop = Connection("<rig1> one out", "<top> top input")
    val connsMid = Connection("<rig2> two out", "<mid> mid input")
    val connsBottom = Connection("<free> three out", "<bottom> bottom input")
    
    val setupBottom = new Setup(Set(connsBottom))
    val setupMid = Setup(Set(connsMid)).withRig("<rig2>", setupBottom)
    val setupTop = Setup(Set(connsTop)).withRig("<rig1>", setupMid).withPosUpdated(List("<rig1>"))
    
    setupTop.setupForPos(List()) should equal (Some(setupTop))
    setupTop.setupForPos(List("<rig1>")) should equal (Some(setupMid))
    setupTop.setupForPos(List("<rig1>", "<rig2>")) should equal (Some(setupBottom))
    setupTop.setupForPos(List("<rig1>", "<rig22>")) should equal (None)
    setupTop.setupForPos(List("<rig1>", "<rig2>", "<rig33>")) should equal (None)
  }

}