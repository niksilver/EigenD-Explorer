package org.pigsaw.eigendmapper


/**
 * Utilities to make our data graphable with Gephi, and the gexf file format.
 */
class Graphable(val conns: Set[Connection]) {
  import Graphable._
  
  /**
   * Create a unified set of connections. This means if any connections
   * carry a port name, then those names are applied wherever those
   * ports are used.
   */
  def unified: Set[Connection] = {
    val ports = conns flatMap { c => List(c.master, c.slave) }
    val namingPorts = ports filter (_.name.nonEmpty)
    val names: Map[String, String] = namingPorts map { p => (p.id -> p.name.get) } toMap

    // Produce an updated version of the port, with names filled in if available.
    def updated(port: Port): Port = {
      if (port.name.nonEmpty) port
      else Port(port.id, names.get(port.id))
    }

    conns map (c => Connection(updated(c.master), updated(c.slave)))
  }

  /**
   * Make a normalised version of this set of connections, in which
   * every port of the form ID &lt;main:agentnameN&gt; is changed to
   * its shorter form of &lt;agentnameN&gt;.
   */
  def normalised: Set[Connection] =
    conns map { _.normalised }

  /**
   * Get all the agent names mentioned in the set of connections,
   * including the angle brackets.
   */
  def agents: Set[String] =
    conns flatMap { _.agents }
  
  /**
   * Get all the ports named in the connections.
   */
  def ports: Set[Port] =
    conns flatMap { c => List(c.master, c.slave) }
  
  /**
   * Get a map from each agent to each agent (strings, including
   * angle brackets.)
   */
  def agentAgentConnections: Set[(String, String)] =
    conns map { c => (c.master.agent getOrElse "UNKNOWN", c.slave.agent getOrElse "UNKNOWN") }
  
  /**
   * Get a map from each agent (a string including angle brackets)
   * to all its ports.
   */
  def agentPortConnections: Set[(String, Port)] =
    conns.ports map { p => ((p.agent getOrElse "UNKNOWN") -> p) }
}

object Graphable {
  implicit def setConnection2Graphable(conns: Set[Connection]): Graphable = new Graphable(conns)
  implicit def listConnection2Graphable(conns: List[Connection]): Graphable = new Graphable(conns.toSet)
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
