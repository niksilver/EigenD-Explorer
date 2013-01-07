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

import Graphable._

@RunWith(classOf[JUnitRunner])
class GraphableSuite extends FunSuite with ShouldMatchers {
  
  test("Agent name (string) XML id") {
    "<alpha>".stringXmlId should equal ("_alpha_")
  }
  
  test("String XML escaped") {
    "<alph'a>".xmlEscaped should equal ("&lt;alph&apos;a&gt;")
  }
  
  test("Agent (string) node XML") {
    "<alpha>".stringNodeXML should equal ("""<node id="_alpha_" label="&lt;alpha&gt;" />""")
  }
  
  test("Agent-port XML id") {
    val agentPort = GAgentPort("<alpha>" -> "<alpha>#4.5")
    agentPort.xmlId should equal ("_alpha__alpha__4.5")
  }
  
  test("Port-agent XML id") {
    val portAgent = GPortAgent("<alpha>#4.5" -> "<alpha>")
    portAgent.xmlId should equal ("_alpha__4.5_alpha_")
  }
  
  test("Agent-agent XML id") {
    val agentAgent = GAgentAgent("<alpha>" -> "<beta>")
    agentAgent.xmlId should equal ("_alpha__beta_")
  }
  
  test("Agent-agent edge XML") {
    val agentAgent = GAgentAgent("<alpha>" -> "<beta>")
    agentAgent.edgeXML should equal ("""<edge id="_alpha__beta_" source="_alpha_" target="_beta_" weight="1" />""")
  }
  
  test("Agent-port edge XML") {
    val agentPort = GAgentPort("<alpha>" -> "<alpha>#4.5")
    agentPort.edgeXML should equal ("""<edge id="_alpha__alpha__4.5" source="_alpha_" target="_alpha__4.5" weight="1" />""")
  }
  
  test("Port-agent edge XML") {
    val portAgent = GPortAgent("<alpha>#4.5" -> "<alpha>")
    portAgent.edgeXML should equal ("""<edge id="_alpha__4.5_alpha_" source="_alpha__4.5" target="_alpha_" weight="1" />""")
  }
  
  test("Port XML id") {
    "<alpha>#4.5".portXmlId should equal ("_alpha__4.5")
  }
  
  test("Connection XML id") {
    Connection("<alpha>#4.5", "<b>#1.1").xmlId should equal ("_alpha__4.5_b__1.1")
  }
  
  test("Port node XML") {
    "<alpha>#4.5".portNodeXML should equal ("""<node id="_alpha__4.5" label="#4.5" />""")
    "<alpha> light out".portNodeXML should equal ("""<node id="_alpha__light_out" label="light out" />""")
  }
  
  test("Connection edge XML") {
    val conn = Connection("<alpha>#4.5", "<b>#1.1")
    conn.edgeXML should equal ("""<edge id="_alpha__4.5_b__1.1" source="_alpha__4.5" target="_b__1.1" weight="1" />""")
  }

}