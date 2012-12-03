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
        Connection(c, d)
    ))
    
    val agents = setup.agents
    
    agents.size should equal (3)
    agents should contain ("<a>")
    agents should contain ("<b>")
    agents should contain ("<c>")
  }
  
  test("Ports") {
    val a = Port("<a>#1.1", None)
    val b = Port("<b>#1.2", Some("b12"))
    val c = Port("<c>#1.3", None)
    val d = Port("d#1.4", None) // No parseable agent name

    val setup = new Setup(Set(
        Connection(a, b),
        Connection(b, c),
        Connection(c, d)
    ))
    
    val ports = setup.ports
    
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

    val setup = new Setup(Set(
        Connection(a, b),
        Connection(b, c1),
        Connection(c1, b),
        Connection(c2, b),
        Connection(c1, d)
    ))
    
    val agAgConns = setup.agentAgentConnections
    
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

    val setup = new Setup(Set(
        Connection(a1, b1),
        Connection(a2, b2)
    ))
    
    val conns2 = setup.agentPortConnections
    
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

    val setup = new Setup(Set(
        Connection(a1, b1),
        Connection(a2, b2)
    ))
    
    val conns2 = setup.agentPortConnections
    
    conns2.size should equal (4)
    conns2 should contain ("<a>" -> a1)
    conns2 should contain ("UNKNOWN" -> a2)
    conns2 should contain ("UNKNOWN" -> b1)
    conns2 should contain ("UNKNOWN" -> b2)
  }

}