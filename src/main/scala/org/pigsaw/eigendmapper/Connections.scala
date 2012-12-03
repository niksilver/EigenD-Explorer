package org.pigsaw.eigendmapper

/**
 * A port in an agent that might be one end of a connection.
 * @param id  Agent name, including angle brackets and ordinal, and port
 *               e.g. "&lt;metronome1&gt#3.6;
 * @param name  The name of the port if known, e.g. "bar beat output"
 */
case class Port(val id: String, val name: Option[String]) {
  /**
   * Generate a normalised version of this port. I.e. If the id is of
   * the form &lt;<main:agentnameN&gt; then it's converted to &lt;agentnameN&gt;.
   */
  def normalised: Port =
    if (id.startsWith("<main:")) Port("<" + id.drop(6), name)
    else this

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
   * Generate a normalised version of this connection. I.e. If the id of either
   * the master or the slave is of the form &lt;<main:agentnameN&gt; then it's
   * converted to &lt;agentnameN&gt;.
   */
  def normalised: Connection = {
    val normMaster = master.normalised
    val normSlave = slave.normalised
    if ((normMaster eq master) && (normSlave eq slave)) this
    else Connection(normMaster, normSlave)
  }

  /**
   * Get the agent names embedded in the master and slave port ids.
   */
  def agents: Set[String] = Set() ++ master.agent.toSeq ++ slave.agent.toSeq
}

/**
 * A particular set of connections
 */
class Setup(val conns: Set[Connection]) {

  /**
   * Get all the agent names mentioned in the set of connections,
   * including the angle brackets.
   */
  lazy val agents: Set[String] =
    conns flatMap { _.agents }
      
  /**
   * Get all the ports named in the connections.
   */
  lazy val ports: Set[Port] =
    conns flatMap { c => List(c.master, c.slave) }
  
  /**
   * Get a map from each agent to each agent (strings, including
   * angle brackets.)
   */
  lazy val agentAgentConnections: Set[(String, String)] =
    conns map { c => (c.master.agent getOrElse "UNKNOWN", c.slave.agent getOrElse "UNKNOWN") }

  /**
   * Get a map from each agent (a string including angle brackets)
   * to all its ports.
   */
  lazy val agentPortConnections: Set[(String, Port)] =
    ports map { p => ((p.agent getOrElse "UNKNOWN") -> p) }

}
