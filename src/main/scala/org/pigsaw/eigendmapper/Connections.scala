package org.pigsaw.eigendmapper

import Preamble._

/**
 * A port in an agent that might be one end of a connection.
 * @param id  Agent name, including angle brackets and ordinal, and port
 *               e.g. "&lt;metronome1&gt#3.6;
 * @param name  The name of the port if known, e.g. "bar beat output"
 */
case class Port(val id: String, val name: Option[String]) {
  /**
   * Generate an unqualified version of this port. I.e. If the id is of
   * the form &lt;<main:agentnameN&gt; then it's converted to &lt;agentnameN&gt;.
   */
  def unqualified: Port = {
    val FullyQualifiedID = """<(.*:)(\w*>.*)""".r
    id match {
      case FullyQualifiedID(_, rest) => Port("<" + rest, name)
      case _ => this
    }
  }

  /**
   * Get the agent name embedded in the port id, including the angle brackets.
   */
  def agent: Option[String] = "(<.*>)".r findFirstIn (id)

  /**
   * Get the name given to the port, or if it's None, the id of the port.
   */
  def nonEmptyName: String = name getOrElse id

  /**
   * Get the name given to the port, or if it's None, the id of the port, and make
   * sure the agent is also included
   */
  def nonEmptyFQName: String =
    if (name.isEmpty) id
    else (agent getOrElse "<UNKNOWN>") + " " + name.get
}

case class Connection(val master: Port, val slave: Port) {
  /**
   * Generate an unqualified version of this connection. I.e. If the id of either
   * the master or the slave is of the form &lt;<main:agentnameN&gt; then it's
   * converted to &lt;agentnameN&gt;.
   */
  def unqualified: Connection = {
    val unqualMaster = master.unqualified
    val unqualSlave = slave.unqualified
    if ((unqualMaster eq master) && (unqualSlave eq slave)) this
    else Connection(unqualMaster, unqualSlave)
  }

  /**
   * Get the agent names embedded in the master and slave port ids.
   */
  def agents: Set[String] = Set() ++ master.agent.toSeq ++ slave.agent.toSeq
}
