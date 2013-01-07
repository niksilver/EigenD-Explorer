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

import Preamble._

object Graphable {
  
  val edgeXMLTemplate = """<edge id="%s" source="%s" target="%s" weight="%s" />"""
  val portAgentEdgeWeight = 1
  val agentAgentEdgeWeight = 1
  val portPortEdgeWeight = 1

  implicit def string2Graphable(s: String) = new Graphable(s)
  implicit def connection2GPortPort(c: Connection) = new GPortPort(c)

  def GPortPort(c: Connection) = new GPortPort(c)
  def GAgentPort(ap: (String, String)) = new GAgentPort(ap)
  def GPortAgent(pa: (String, String)) = new GPortAgent(pa)
  def GAgentAgent(aa: (String, String)) = new GAgentAgent(aa)

  val gexfHeader =
    """<?xml version="1.0" encoding="UTF-8"?>
      |    <gexf xmlns="http://www.gexf.net/1.2draft" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.gexf.net/1.2draft http://www.gexf.net/1.2draft/gexf.xsd" version="1.2">
      |        <graph mode="static" defaultedgetype="directed">""".stripMargin
  val gexfFooter =
    """    </graph>
      |</gexf>""".stripMargin

   class Graphable(s: String) {
    def stringXmlId: String = "[^A-Za-z0-9.]".r replaceAllIn (s, "_")

    def xmlEscaped: String =
      s.replaceAllLiterally("<", "&lt;").replaceAllLiterally(">", "&gt;").replaceAllLiterally("'", "&apos;")

    def stringNodeXML: String =
      """<node id="%s" label="%s" />""".format(s.stringXmlId, s.xmlEscaped)

    lazy val portXmlId: String = PortID(s).id.stringXmlId

    lazy val portNodeXML: String =
      """<node id="%s" label="%s" />""".format(s.portXmlId, s.nodeLabelWithHash.xmlEscaped)

  }

  class GPortPort(c: Connection) {
    lazy val xmlId: String = c.master.portXmlId + c.slave.portXmlId

    lazy val edgeXML: String =
      edgeXMLTemplate.format(xmlId, c.master.portXmlId, c.slave.portXmlId, portPortEdgeWeight)
  }

  class GAgentPort(ap: (String, String)) {
    lazy val xmlId: String = ap._1.stringXmlId + ap._2.portXmlId
    
    lazy val edgeXML: String =      
      edgeXMLTemplate.format(xmlId, ap._1.stringXmlId, ap._2.portXmlId, portAgentEdgeWeight)
  }

  class GPortAgent(pa: (String, String)) {
    lazy val xmlId: String = pa._1.portXmlId + pa._2.stringXmlId
    
    lazy val edgeXML: String =
      edgeXMLTemplate.format(xmlId, pa._1.portXmlId, pa._2.stringXmlId, portAgentEdgeWeight)
  }

  class GAgentAgent(aa: (String, String)) {
    lazy val xmlId: String = aa._1.stringXmlId + aa._2.stringXmlId

    lazy val edgeXML: String =
      edgeXMLTemplate.format(xmlId, aa._1.stringXmlId, aa._2.stringXmlId, agentAgentEdgeWeight)
  }  
}
