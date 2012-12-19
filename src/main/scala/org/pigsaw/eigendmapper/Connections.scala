package org.pigsaw.eigendmapper

import Preamble._

/**
 * A connection between two ports, the master and the slave.
 * Both ports should be of the form `<agent1>#11.2.33`.
 * @param master  The port the connection is from.
 * @param slave  The port the connection is to.
 */
case class Connection(val master: String, val slave: String) {
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
  def agents: Set[String] = Set() + master.agent + slave.agent
  
  /**
   * See if one port in this connection has the given position
   */
  def hasPos(p: List[String]): Boolean =
    (master.hasPos(p)) || (slave.hasPos(p))
}
