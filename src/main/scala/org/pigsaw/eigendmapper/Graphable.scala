package org.pigsaw.eigendmapper

import Preamble._

object Graphable {

  implicit def string2Graphable(s: String) = new Graphable(s)
  implicit def connection2GraphableConnection(c: Connection) = new GConnection(c)

  def GAgentPort(ap: (String, String)) = new GAgentPort(ap)
  def GAgentAgent(aa: (String, String)) = new GAgentAgent(aa)

  val gexfHeader =
    """<?xml version="1.0" encoding="UTF-8"?>
      |    <gexf xmlns="http://www.gexf.net/1.2draft" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.gexf.net/1.2draft http://www.gexf.net/1.2draft/gexf.xsd" version="1.2">
      |        <graph mode="static" defaultedgetype="directed">""".stripMargin + "\n"
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
      """<node id="%s" label="%s" />""".format(s.portXmlId, s.nodeLabel.xmlEscaped)

  }

  class GConnection(c: Connection) {
    lazy val xmlId: String = c.master.portXmlId + c.slave.portXmlId

    lazy val edgeXML: String = {
      val template = """<edge id="%s" source="%s" target="%s" weight="1" />"""
      template.format(c.xmlId, c.master.portXmlId, c.slave.portXmlId)
    }
  }

  class GAgentPort(ap: (String, String)) {
    lazy val xmlId: String = ap._1.stringXmlId + ap._2.portXmlId
    
    lazy val edgeXML: String = {
      val template = """<edge id="%s" source="%s" target="%s" weight="5" />"""
      template.format(GAgentPort(ap).xmlId, ap._1.stringXmlId, ap._2.portXmlId)
    }
  }

  class GAgentAgent(aa: (String, String)) {
    lazy val xmlId: String = aa._1.stringXmlId + aa._2.stringXmlId

    lazy val edgeXML: String = {
      val template = """<edge id="%s" source="%s" target="%s" weight="3" />"""
      template.format(GAgentAgent(aa).xmlId, aa._1.stringXmlId, aa._2.stringXmlId)
    }
  }  
}
