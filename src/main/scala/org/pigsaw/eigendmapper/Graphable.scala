package org.pigsaw.eigendmapper


/**
 * Utilities to make our data graphable with Gephi, and the gexf file format.
 */
class Graphable(val conns: Set[Connection]) {
  import Graphable._
}

object Graphable {
  implicit def string2GraphableString(s: String) = new GString(s)
  implicit def port2GraphablePort(p: Port) = new GPort(p)
  implicit def connection2GraphableConnection(c: Connection) = new GConnection(c)
  implicit def agentPort2GraphableAgentPort(ap: (String, Port)) = new GAgentPort(ap)
  implicit def agentAgent2GraphableAgentAgent(aa: (String, String)) = new GAgentAgent(aa)

  val gexfHeader =
    """<?xml version="1.0" encoding="UTF-8"?>
      |    <gexf xmlns="http://www.gexf.net/1.2draft" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.gexf.net/1.2draft http://www.gexf.net/1.2draft/gexf.xsd" version="1.2">
      |        <graph mode="static" defaultedgetype="directed">""".stripMargin + "\n"
  val gexfFooter =
    """    </graph>
      |</gexf>""".stripMargin

  class GString(s: String) {
    lazy val xmlId: String = "[^A-Za-z0-9.]".r replaceAllIn (s, "_")

    lazy val xmlEscaped: String =
      s.replaceAllLiterally("<", "&lt;").replaceAllLiterally(">", "&gt;").replaceAllLiterally("'", "&apos;")

    lazy val nodeXML: String = {
      """<node id="%s" label="%s" />""".format(s.xmlId, s.xmlEscaped)
    }
  }

  class GPort(p: Port) {
    lazy val xmlId: String = p.id.xmlId

    lazy val nodeXML: String = {
      val label = p.name getOrElse p.id
      """<node id="%s" label="%s" />""".format(p.xmlId, label.xmlEscaped)
    }
  }

  class GConnection(c: Connection) {
    lazy val xmlId: String = c.master.xmlId + c.slave.xmlId

    lazy val edgeXML: String = {
      val template = """<edge id="%s" source="%s" target="%s" weight="1" />"""
      template.format(c.xmlId, c.master.xmlId, c.slave.xmlId)
    }
  }

  class GAgentPort(ap: (String, Port)) {
    lazy val xmlId: String = ap._1.xmlId + ap._2.xmlId
    
    lazy val edgeXML: String = {
      val template = """<edge id="%s" source="%s" target="%s" weight="5" />"""
      template.format(ap.xmlId, ap._1.xmlId, ap._2.xmlId)
    }
  }

  class GAgentAgent(aa: (String, String)) {
    lazy val xmlId: String = aa._1.xmlId + aa._2.xmlId

    lazy val edgeXML: String = {
      val template = """<edge id="%s" source="%s" target="%s" weight="3" />"""
      template.format(aa.xmlId, aa._1.xmlId, aa._2.xmlId)
    }
  }  
}
