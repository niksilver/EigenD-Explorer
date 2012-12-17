package org.pigsaw.eigendmapper

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers

import org.pigsaw.eigendmapper.Graphable._

@RunWith(classOf[JUnitRunner])
class GraphableSuite extends FunSuite with ShouldMatchers {
  
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
    val agentPort = ("<alpha>" -> DeprecatedPort("<alpha>#4.5", None))
    agentPort.xmlId should equal ("_alpha__alpha__4.5")
  }
  
  test("Agent-agent XML id") {
    val agentAgent = ("<alpha>" -> "<beta>")
    agentAgent.xmlId should equal ("_alpha__beta_")
  }
  
  test("Agent-agent edge XML") {
    val agentAgent = ("<alpha>" -> "<beta>")
    agentAgent.edgeXML should equal ("""<edge id="_alpha__beta_" source="_alpha_" target="_beta_" weight="3" />""")
  }
  
  test("Agent-port edge XML") {
    val agentPort = ("<alpha>" -> DeprecatedPort("<alpha>#4.5", None))
    agentPort.edgeXML should equal ("""<edge id="_alpha__alpha__4.5" source="_alpha_" target="_alpha__4.5" weight="5" />""")
  }
  
  test("Port XML id") {
    DeprecatedPort("<alpha>#4.5", None).xmlId should equal ("_alpha__4.5")
  }
  
  test("Connection XML id") {
    Connection(DeprecatedPort("<alpha>#4.5", None), DeprecatedPort("<b>#1.1", None)).xmlId should equal ("_alpha__4.5_b__1.1")
  }
  
  test("Port node XML") {
    DeprecatedPort("<alpha>#4.5", None).nodeXML should equal ("""<node id="_alpha__4.5" label="&lt;alpha&gt;#4.5" />""")
    DeprecatedPort("<alpha>#4.5", Some("light out")).nodeXML should equal ("""<node id="_alpha__4.5" label="light out" />""")
  }
  
  test("Connection edge XML") {
    val conn = Connection(DeprecatedPort("<alpha>#4.5", None), DeprecatedPort("<b>#1.1", None))
    conn.edgeXML should equal ("""<edge id="_alpha__4.5_b__1.1" source="_alpha__4.5" target="_b__1.1" weight="1" />""")
  }

}