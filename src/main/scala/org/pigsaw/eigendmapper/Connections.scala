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
