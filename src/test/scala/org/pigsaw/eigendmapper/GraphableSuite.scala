package org.pigsaw.eigendmapper

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers

import org.pigsaw.eigendmapper.Graphable._

@RunWith(classOf[JUnitRunner])
class GraphableSuite extends FunSuite with ShouldMatchers {

  test("Unify - Add a slave name, expect a slave updated") {
    val port_a_unnamed = Port("<a>#1.1", None)
    val port_b_named = Port("<b>#1.2", Some("b12"))
    val port_b_unnamed = Port("<b>#1.2", None)
    val port_c_unnamed = Port("<c>#1.3", None)

    val conn_aubu = Connection(port_a_unnamed, port_b_unnamed)

    val connSet1 = Set(conn_aubu)

    connSet1.size should equal (1)
    connSet1 should contain (conn_aubu)

    val conn_cubn = Connection(port_c_unnamed, port_b_named) // We'll add this
    val conn_aubn = Connection(port_a_unnamed, port_b_named) // This should get created
    
    val connSet2 = (connSet1 + conn_cubn).unified

    connSet2.size should equal (2)
    connSet2 should contain (conn_cubn)
    connSet2 should not contain (conn_aubu)
    connSet2 should contain (conn_aubn)
  }

  test("Unify - Add a slave name, expect a master updated") {
    val port_a_unnamed = Port("<a>#1.1", None)
    val port_b_named = Port("<b>#1.2", Some("b12"))
    val port_b_unnamed = Port("<b>#1.2", None)
    val port_c_unnamed = Port("<c>#1.3", None)

    val conn_buau = Connection(port_b_unnamed, port_a_unnamed)

    val connSet1 = Set(conn_buau)

    connSet1.size should equal (1)
    connSet1 should contain (conn_buau)

    val conn_cubn = Connection(port_c_unnamed, port_b_named) // We'll add this
    val conn_bnau = Connection(port_b_named, port_a_unnamed) // This should get created
    
    val connSet2 = (connSet1 + conn_cubn).unified

    connSet2.size should equal (2)
    connSet2 should contain (conn_cubn)
    connSet2 should not contain (conn_buau)
    connSet2 should contain (conn_bnau)
  }

  test("Unify - Add a master name, expect a slave updated") {
    val port_a_unnamed = Port("<a>#1.1", None)
    val port_b_named = Port("<b>#1.2", Some("b12"))
    val port_b_unnamed = Port("<b>#1.2", None)
    val port_c_unnamed = Port("<c>#1.3", None)

    val conn_aubu = Connection(port_a_unnamed, port_b_unnamed)

    val connSet1 = Set(conn_aubu)

    connSet1.size should equal (1)
    connSet1 should contain (conn_aubu)

    val conn_bncu = Connection(port_b_named, port_c_unnamed) // We'll add this
    val conn_aubn = Connection(port_a_unnamed, port_b_named) // This should get created
    
    val connSet2 = (connSet1 + conn_bncu).unified

    connSet2.size should equal (2)
    connSet2 should contain (conn_bncu)
    connSet2 should not contain (conn_aubu)
    connSet2 should contain (conn_aubn)
  }

  test("Unify - Add a master name, expect a master updated") {
    val port_a_unnamed = Port("<a>#1.1", None)
    val port_b_named = Port("<b>#1.2", Some("b12"))
    val port_b_unnamed = Port("<b>#1.2", None)
    val port_c_unnamed = Port("<c>#1.3", None)

    val conn_buau = Connection(port_b_unnamed, port_a_unnamed)

    val connSet1 = Set(conn_buau)

    connSet1.size should equal (1)
    connSet1 should contain (conn_buau)

    val conn_bncu = Connection(port_b_named, port_c_unnamed) // We'll add this
    val conn_bnau = Connection(port_b_named, port_a_unnamed) // This should get created
    
    val connSet2 = (connSet1 + conn_bncu).unified

    connSet2.size should equal (2)
    connSet2 should contain (conn_bncu)
    connSet2 should not contain (conn_buau)
    connSet2 should contain (conn_bnau)
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
    		Connection(b_short, c_long)
    )
    
    val conns2 = conns.normalised
    
    conns2.size should equal (2)
    conns2 should not contain (Connection(a_short, b_long))
    conns2 should not contain (Connection(b_long, c_long))
    conns2 should not contain (Connection(b_short, c_long))
    conns2 should contain (Connection(a_short, b_short))
    conns2 should contain (Connection(b_short, c_short))
    
  }
  
  test("Agents") {
    val a = Port("<a>#1.1", None)
    val b = Port("<b>#1.2", Some("b12"))
    val c = Port("<c>#1.3", None)
    val d = Port("d#1.4", None) // No parseable agent name

    val conns = Set(
        Connection(a, b),
        Connection(b, c),
        Connection(c, d)
    )
    
    val agents = conns.agents
    
    agents.size should equal (3)
    agents should contain ("<a>")
    agents should contain ("<b>")
    agents should contain ("<c>")
  }
  
  test("Agent name (string) XML id") {
    "<alpha>".xmlId should equal ("_alpha_")
  }
  
  test("String XML escaped") {
    "<alph'a>".xmlEscaped should equal ("&lt;alph&apos;a&gt;")
  }
  
  test("Agent (string) node XML") {
    "<alpha>".nodeXML should equal ("""<node id="_alpha_" label="&lt;alpha&gt;" />""")
  }
  
  test("Agent-port XML id") {
    val agentPort = ("<alpha>" -> Port("<alpha>#4.5", None))
    agentPort.xmlId should equal ("_alpha__alpha__4.5")
  }
  
  test("Agent-port edge XML") {
    val agentPort = ("<alpha>" -> Port("<alpha>#4.5", None))
    agentPort.edgeXML should equal ("""<edge id="_alpha__alpha__4.5" source="_alpha_" target="_alpha__4.5" weight="5" />""")
  }
  
  test("Port XML id") {
    Port("<alpha>#4.5", None).xmlId should equal ("_alpha__4.5")
  }
  
  test("Connection XML id") {
    Connection(Port("<alpha>#4.5", None), Port("<b>#1.1", None)).xmlId should equal ("_alpha__4.5_b__1.1")
  }
  
  test("Port node XML") {
    Port("<alpha>#4.5", None).nodeXML should equal ("""<node id="_alpha__4.5" label="&lt;alpha&gt;#4.5" />""")
    Port("<alpha>#4.5", Some("light out")).nodeXML should equal ("""<node id="_alpha__4.5" label="light out" />""")
  }
  
  test("Connection edge XML") {
    val conn = Connection(Port("<alpha>#4.5", None), Port("<b>#1.1", None))
    conn.edgeXML should equal ("""<edge id="_alpha__4.5_b__1.1" source="_alpha__4.5" target="_b__1.1" weight="1" />""")
  }
  
  test("Ports") {
    val a = Port("<a>#1.1", None)
    val b = Port("<b>#1.2", Some("b12"))
    val c = Port("<c>#1.3", None)
    val d = Port("d#1.4", None) // No parseable agent name

    val conns = Set(
        Connection(a, b),
        Connection(b, c),
        Connection(c, d)
    )
    
    val ports = conns.ports
    
    ports.size should equal (4)
    ports should contain (a)
    ports should contain (b)
    ports should contain (c)
    ports should contain (d)
  }
  
  test("Agent-agent connections") {
    val a = Port("<a>#1.1", None)
    val b = Port("<b>#1.2", Some("b12"))
    val c1 = Port("<c>#1.3", None)
    val c2 = Port("<c>#2.3", None)
    val d = Port("d#1.4", None) // No parseable agent name

    val conns = Set(
        Connection(a, b),
        Connection(b, c1),
        Connection(c1, b),
        Connection(c2, b),
        Connection(c1, d)
    )
    
    val agAgConns = conns.agentAgentConnections
    
    agAgConns.size should equal (4)
    agAgConns should contain ("<a>", "<b>")
    agAgConns should contain ("<b>", "<c>")
    agAgConns should contain ("<c>", "<b>")
    agAgConns should contain ("<c>", "UNKNOWN")
  }
  
  test("Agent-port connections") {
    val a1 = Port("<a>#1.1", None)
    val a2 = Port("<a>#1.2", Some("b12"))
    val b1 = Port("<b>#1.1", None)
    val b2 = Port("<b>#1.2", None)

    val conns = Set(
        Connection(a1, b1),
        Connection(a2, b2)
    )
    
    val conns2 = conns.agentPortConnections
    
    conns2.size should equal (4)
    conns2 should contain ("<a>" -> a1)
    conns2 should contain ("<a>" -> a2)
    conns2 should contain ("<b>" -> b1)
    conns2 should contain ("<b>" -> b2)
  }
  
  test("Agent-port connections - with unparseable agent name") {
    val a1 = Port("<a>#1.1", None)
    val a2 = Port("a#1.2", Some("b12"))
    val b1 = Port("b#1.1", None)
    val b2 = Port("b#1.2", None)

    val conns = Set(
        Connection(a1, b1),
        Connection(a2, b2)
    )
    
    val conns2 = conns.agentPortConnections
    
    conns2.size should equal (4)
    conns2 should contain ("<a>" -> a1)
    conns2 should contain ("UNKNOWN" -> a2)
    conns2 should contain ("UNKNOWN" -> b1)
    conns2 should contain ("UNKNOWN" -> b2)
  }

}