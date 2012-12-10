package org.pigsaw.eigendmapper

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers

import org.pigsaw.eigendmapper.Graphable._

@RunWith(classOf[JUnitRunner])
class SetupSuite extends FunSuite with ShouldMatchers {

  test("Agents") {
    val a = Port("<a>#1.1", None)
    val b = Port("<b>#1.2", Some("b12"))
    val c = Port("<c>#1.3", None)
    val d = Port("d#1.4", None) // No parseable agent name

    val setup = new Setup(Set(
      Connection(a, b),
      Connection(b, c),
      Connection(c, d)))

    val agents = setup.agents

    agents.size should equal(3)
    agents should contain("<a>")
    agents should contain("<b>")
    agents should contain("<c>")
  }

  test("Ports") {
    val a = Port("<a>#1.1", None)
    val b = Port("<b>#1.2", Some("b12"))
    val c = Port("<c>#1.3", None)
    val d = Port("d#1.4", None) // No parseable agent name

    val setup = new Setup(Set(
      Connection(a, b),
      Connection(b, c),
      Connection(c, d)))

    val ports = setup.ports

    ports.size should equal(4)
    ports should contain(a)
    ports should contain(b)
    ports should contain(c)
    ports should contain(d)
  }

  test("Agent-agent connections") {
    val a = Port("<a>#1.1", None)
    val b = Port("<b>#1.2", Some("b12"))
    val c1 = Port("<c>#1.3", None)
    val c2 = Port("<c>#2.3", None)
    val d = Port("d#1.4", None) // No parseable agent name

    val setup = new Setup(Set(
      Connection(a, b),
      Connection(b, c1),
      Connection(c1, b),
      Connection(c2, b),
      Connection(c1, d)))

    val agAgConns = setup.agentAgentConnections

    agAgConns.size should equal(4)
    agAgConns should contain("<a>", "<b>")
    agAgConns should contain("<b>", "<c>")
    agAgConns should contain("<c>", "<b>")
    agAgConns should contain("<c>", "UNKNOWN")
  }

  test("Agent-port connections") {
    val a1 = Port("<a>#1.1", None)
    val a2 = Port("<a>#1.2", Some("b12"))
    val b1 = Port("<b>#1.1", None)
    val b2 = Port("<b>#1.2", None)

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

  test("Agent-port connections - with unparseable agent name") {
    val a1 = Port("<a>#1.1", None)
    val a2 = Port("a#1.2", Some("b12"))
    val b1 = Port("b#1.1", None)
    val b2 = Port("b#1.2", None)

    val setup = new Setup(Set(
      Connection(a1, b1),
      Connection(a2, b2)))

    val conns2 = setup.agentPortConnections

    conns2.size should equal(4)
    conns2 should contain("<a>" -> a1)
    conns2 should contain("UNKNOWN" -> a2)
    conns2 should contain("UNKNOWN" -> b1)
    conns2 should contain("UNKNOWN" -> b2)
  }

  test("Unify - Add a slave name, expect a slave updated") {
    val port_a_unnamed = Port("<a>#1.1", None)
    val port_b_named = Port("<b>#1.2", Some("b12"))
    val port_b_unnamed = Port("<b>#1.2", None)
    val port_c_unnamed = Port("<c>#1.3", None)

    val conn_aubu = Connection(port_a_unnamed, port_b_unnamed)

    val connSet1 = Set(conn_aubu)

    connSet1.size should equal(1)
    connSet1 should contain(conn_aubu)

    val conn_cubn = Connection(port_c_unnamed, port_b_named) // We'll add this
    val conn_aubn = Connection(port_a_unnamed, port_b_named) // This should get created

    val connSet2 = new Setup(connSet1 + conn_cubn).unified.conns

    connSet2.size should equal(2)
    connSet2 should contain(conn_cubn)
    connSet2 should not contain (conn_aubu)
    connSet2 should contain(conn_aubn)
  }

  test("Unify - Add a slave name, expect a master updated") {
    val port_a_unnamed = Port("<a>#1.1", None)
    val port_b_named = Port("<b>#1.2", Some("b12"))
    val port_b_unnamed = Port("<b>#1.2", None)
    val port_c_unnamed = Port("<c>#1.3", None)

    val conn_buau = Connection(port_b_unnamed, port_a_unnamed)

    val connSet1 = Set(conn_buau)

    connSet1.size should equal(1)
    connSet1 should contain(conn_buau)

    val conn_cubn = Connection(port_c_unnamed, port_b_named) // We'll add this
    val conn_bnau = Connection(port_b_named, port_a_unnamed) // This should get created

    val connSet2 = new Setup(connSet1 + conn_cubn).unified.conns

    connSet2.size should equal(2)
    connSet2 should contain(conn_cubn)
    connSet2 should not contain (conn_buau)
    connSet2 should contain(conn_bnau)
  }

  test("Unify - Add a master name, expect a slave updated") {
    val port_a_unnamed = Port("<a>#1.1", None)
    val port_b_named = Port("<b>#1.2", Some("b12"))
    val port_b_unnamed = Port("<b>#1.2", None)
    val port_c_unnamed = Port("<c>#1.3", None)

    val conn_aubu = Connection(port_a_unnamed, port_b_unnamed)

    val connSet1 = Set(conn_aubu)

    connSet1.size should equal(1)
    connSet1 should contain(conn_aubu)

    val conn_bncu = Connection(port_b_named, port_c_unnamed) // We'll add this
    val conn_aubn = Connection(port_a_unnamed, port_b_named) // This should get created

    val connSet2 = new Setup(connSet1 + conn_bncu).unified.conns

    connSet2.size should equal(2)
    connSet2 should contain(conn_bncu)
    connSet2 should not contain (conn_aubu)
    connSet2 should contain(conn_aubn)
  }

  test("Unify - Add a master name, expect a master updated") {
    val port_a_unnamed = Port("<a>#1.1", None)
    val port_b_named = Port("<b>#1.2", Some("b12"))
    val port_b_unnamed = Port("<b>#1.2", None)
    val port_c_unnamed = Port("<c>#1.3", None)

    val conn_buau = Connection(port_b_unnamed, port_a_unnamed)

    val connSet1 = Set(conn_buau)

    connSet1.size should equal(1)
    connSet1 should contain(conn_buau)

    val conn_bncu = Connection(port_b_named, port_c_unnamed) // We'll add this
    val conn_bnau = Connection(port_b_named, port_a_unnamed) // This should get created

    val connSet2 = new Setup(connSet1 + conn_bncu).unified.conns

    connSet2.size should equal(2)
    connSet2 should contain(conn_bncu)
    connSet2 should not contain (conn_buau)
    connSet2 should contain(conn_bnau)
  }

  test("Normalise port IDs (cut the 'main:' in <main:agentname3>)") {
    val a_short = Port("<a>#1.1", None)
    val a_long = Port("<main:a>#1.1", None)
    val b_short = Port("<b>#1.2", Some("b12"))
    val b_long = Port("<main:b>#1.2", Some("b12"))
    val c_short = Port("<c>#1.3", None)
    val c_long = Port("<main:c>#1.3", None)

    val conns = Set(
      Connection(a_short, b_long),
      Connection(b_long, c_long),
      Connection(b_short, c_long))

    val conns2 = new Setup(conns).normalised.conns

    conns2.size should equal(2)
    conns2 should not contain (Connection(a_short, b_long))
    conns2 should not contain (Connection(b_long, c_long))
    conns2 should not contain (Connection(b_short, c_long))
    conns2 should contain(Connection(a_short, b_short))
    conns2 should contain(Connection(b_short, c_short))

  }

  test("Equals") {
    // One setup
    val conn1 = Connection(Port("<rig3>#3.3", Some("three")), Port("<fff>#5.5", Some("five")))
    val setup1 = Setup(Set(conn1))

    val rigConn1 = Connection(Port("<sss>#7.7", None), Port("<other>#1.1", Some("other")))
    val rigSetup1 = Setup(Set(rigConn1))

    val setup1WithRig = setup1.withRig("<rig3>", rigSetup1)

    // Identical setup with different vals
    val conn2 = Connection(Port("<rig3>#3.3", Some("three")), Port("<fff>#5.5", Some("five")))
    val setup2 = Setup(Set(conn2))

    val rigConn2 = Connection(Port("<sss>#7.7", None), Port("<other>#1.1", Some("other")))
    val rigSetup2 = Setup(Set(rigConn2))

    val setup2WithRig = setup2.withRig("<rig3>", rigSetup2)

    setup1WithRig should equal(setup2WithRig)
  }
  
  test("Equals - with pos") {
    val setupBasic1 = new Setup(Set())
    val setupBasic2 = new Setup(Set())
    val setupWithPos1 = new Setup(Set(), Map(), List("<rig1>"))
    val setupWithPos2 = new Setup(Set(), Map(), List("<rig1>"))
    
    setupBasic1 should equal (setupBasic2)
    setupWithPos1 should equal (setupWithPos2)
    
    setupBasic1 should not equal (setupWithPos1)
    setupWithPos1 should not equal (setupBasic1)
  }

  test("Apply - Should allow empty setup with no params") {
    val setup = Setup()
    setup.agents.size should be(0)
  }

  test("Apply - Make sure it normalises") {
    val a_short = Port("<a>#1.1", None)
    val a_long = Port("<main:a>#1.1", None)
    val b_short = Port("<b>#1.2", Some("b12"))
    val b_long = Port("<main:b>#1.2", Some("b12"))
    val c_short = Port("<c>#1.3", None)
    val c_long = Port("<main:c>#1.3", None)

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

  test("Apply - Make sure it unifies") {
    val port_a_unnamed = Port("<a>#1.1", None)
    val port_b_named = Port("<b>#1.2", Some("b12"))
    val port_b_unnamed = Port("<b>#1.2", None)
    val port_c_unnamed = Port("<c>#1.3", None)

    val conn_buau = Connection(port_b_unnamed, port_a_unnamed)
    val conn_bncu = Connection(port_b_named, port_c_unnamed) // We'll add this
    val conn_bnau = Connection(port_b_named, port_a_unnamed) // This should get created

    val conns = Setup(Set(conn_buau, conn_bncu)).conns

    conns.size should equal(2)
    conns should contain(conn_bncu)
    conns should not contain (conn_buau)
    conns should contain(conn_bnau)
  }

  test("Rigs - Detects rig names") {
    val conn1 = Connection(Port("<rig3>#3.3", Some("three")), Port("<rig5>#5.5", Some("five")))
    val conn2 = Connection(Port("<rig7>#7.7", None), Port("<other>#1.1", Some("other")))
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
    val conn = Connection(Port("<ttt>#3.3", Some("three")), Port("<fff>#5.5", Some("five")))
    val setup = Setup(Set(conn))

    val rigConn = Connection(Port("<sss>#7.7", None), Port("<other>#1.1", Some("other")))
    val rigSetup = Setup(Set(rigConn))

    val setup2 = setup.withRig("<rig1>", rigSetup)

    setup2.rigSetups.size should equal(1)
    setup2.rigSetups should contain("<rig1>" -> rigSetup)
  }

  test("Rigs - Can add two rigs") {
    val conn = Connection(Port("<ttt>#3.3", Some("three")), Port("<fff>#5.5", Some("five")))
    val setup = Setup(Set(conn))

    val rigConn1 = Connection(Port("<sss>#7.7", None), Port("<other>#1.1", Some("other")))
    val rigSetup1 = Setup(Set(rigConn1))

    val rigConn2 = Connection(Port("<eee>#8.8", None), Port("<other>#2.2", Some("other")))
    val rigSetup2 = Setup(Set(rigConn2))

    val setup2 = setup.withRig("<rig1>", rigSetup1).withRig("<rig2>", rigSetup2)

    setup2.rigSetups.size should equal(2)
    setup2.rigSetups should contain("<rig1>" -> rigSetup1)
    setup2.rigSetups should contain("<rig2>" -> rigSetup2)
  }
  
  test("Rigs - Adding rig preserves position") {
    val conn = Connection(Port("<rig1>#1.1", Some("one")), Port("<fff>#5.5", Some("five")))
    val setup = new Setup(Set(conn), Map(), List("<rigX>"))

    val rigConn = Connection(Port("<sss>#7.7", None), Port("<other>#1.1", Some("other")))
    val rigSetup = Setup(Set(rigConn))

    // This should be uncontroversial
    setup.pos should equal (List("<rigX>"))
    
    val setupV2 = setup.withRig("<rig1>", rigSetup)

    // This should not have changed in the revised setup
    setupV2.pos should equal (List("<rigX>"))
  }
  
  test("Position - Can update position") {
    val conn = Connection(Port("<rig1>#1.1", Some("one")), Port("<fff>#5.5", Some("five")))
    val setup = new Setup(Set(conn))
    
    setup.pos should equal (List())
    
    val setupV2 = setup.withPosUpdated(List("<rig1>"))
    
    setupV2.pos should equal (List("<rig1>"))
    setupV2.conns should equal (Set(conn))
  }
  
  test("withConnsReplaced - No args") {
    val conn = Connection(Port("<rig1>#1.1", Some("one")), Port("<fff>#5.5", Some("five")))
    val setup = Setup(Set(conn))

    val rigConn1 = Connection(Port("<sss>#7.7", None), Port("<other>#1.1", Some("other")))
    val rigSetup1 = Setup(Set(rigConn1))

    val setupV2 = setup.withRig("<rig1>", rigSetup1).withRig("<rig2>", rigSetup1)
    
    // Just checking some basics are right before we call the
    // method under test
    
    setupV2.conns should equal (Set(conn))
    setupV2.rigs should equal (Set("<rig1>"))
    setupV2.rigSetups("<rig1>") should equal (rigSetup1)
    
    val newConn = Connection(Port("<rig1>#1.2", Some("one")), Port("<ggg>#7.7", Some("seven")))
    val setupV3 = setupV2.withConnsReplaced(Set(newConn))
    
    setupV3.conns should equal (Set(newConn))
    setupV3.rigs should equal (Set("<rig1>"))
    setupV3.rigSetups("<rig1>") should equal (rigSetup1)
  }
  
  test("withConnsReplaced - Replacing a specific rig's conns - bottom of hierarchy") {
    val connsTop = Connection(Port("<rig1>#1.1", Some("one out")), Port("<top>#5.5", Some("top input")))
    val connsMid = Connection(Port("<rig2>#2.2", Some("two out")), Port("<mid>#7.7", Some("mid input")))
    val connsBottom = Connection(Port("<lower>#3.3", Some("three out")), Port("<bottom>#7.7", Some("bottom input")))
    
    val setupBottom = new Setup(Set(connsBottom))
    val setupMid = new Setup(Set(connsMid), Map("<rig2>" -> setupBottom), List())
    val setupTop = new Setup(Set(connsTop), Map("<rig1>" -> setupMid), List("<rig1>", "<rig2>"))

    val connsBottom2 = Connection(Port("<lower2>#32.32", Some("three out2")), Port("<bottom2>#72.72", Some("bottom input2")))

    val setupTop2 = setupTop.withConnsReplaced(List("<rig1>", "<rig2>"), Set(connsBottom2))
    
    setupTop2.conns should equal (Set(connsTop))
    setupTop2.rigSetups.keySet should equal (Set("<rig1>"))
    setupTop2.pos should equal (List("<rig1>", "<rig2>"))
    
    setupTop2.rigSetups("<rig1>").conns should equal (Set(connsMid))
    setupTop2.rigSetups("<rig1>").rigSetups.keys should equal (Set("<rig2>"))
    
    setupTop2.rigSetups("<rig1>").rigSetups("<rig2>").conns should equal (Set(connsBottom2))
  }

  ignore("withConnsReplaced - Replacing a specific rig's conns - middle of hierarchy") {}
  ignore("withConnsReplaced - Replacing a specific rig's conns - top of hierarchy") {}
  ignore("withConnsReplaced - Normalises and canonicalises conns") {}

  ignore("withConnsReplaced - Preserves position") {}

}