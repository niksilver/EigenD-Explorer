package org.pigsaw.eigendmapper

import java.util.regex.Pattern

/**
 * A particular set of connections
 * @param conns0  The set of connections in this setup.
 * @param rigSetups  The rigs setups inside this one.
 *     Each one is mapped from its name, such as &lt;rig2&gt;.
 * @param pos  The position in the rig hierarchy in which we're currently
 *     interested. An empty list means the top level; List("&lt;rig3&gt")
 *     means rig3 within the top level, and so on.
 */
class Setup(val conns0: Set[Connection],
  val rigSetups0: Map[String, Setup],
  val pos: List[String]) {

  lazy val conns: Set[Connection] = unified(normalised(conns0))

  lazy val rigSetups: Map[String, Setup] = {
    rigs map { name =>
      rigSetups0.get(name) match {
        case Some(setup) => (name -> setup)
        case None        => (name -> Setup())
      }
    } toMap
  }

  /**
   * A setup with no internal rig setups
   */
  def this(conns: Set[Connection]) = this(conns, Map(), List())

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

  /**
   * Create a unified set of connections. This means if any connections
   * carry a port name, then those names are applied wherever those
   * ports are used.
   */
  private def unified(cos: Set[Connection]): Set[Connection] = {
    val ports = cos flatMap { c => List(c.master, c.slave) }
    val namingPorts = ports filter (_.name.nonEmpty)
    val names: Map[String, String] = namingPorts map { p => (p.id -> p.name.get) } toMap

    // Produce an updated version of the port, with names filled in if available.
    def updated(port: Port): Port = {
      if (port.name.nonEmpty) port
      else Port(port.id, names.get(port.id))
    }

    cos map (c => Connection(updated(c.master), updated(c.slave)))
  }

  /**
   * Make a normalised version of a set of connections, in which
   * every port of the form ID &lt;main:agentnameN&gt; is changed to
   * its shorter form of &lt;agentnameN&gt;.
   */
  private def normalised(c: Set[Connection]): Set[Connection] =
    c map { _.normalised }

  /**
   * The rigs in this setup. E.g. <code>"&lt;rig2;&gt"</code>.
   */
  def rigs: Set[String] =
    agents filter { Pattern.matches("<rig\\d+>", _) }

  /**
   * Create a setup just like this, but with a rig setup inside.
   */
  def withRig(rig: String, setup: Setup): Setup =
    new Setup(conns, rigSetups + (rig -> setup), pos)

  /**
   * Create a setup just like this, but the with connections replaced.
   * @param conns2  The new connections.
   */
  def withConnsReplaced(conns2: Set[Connection]): Setup =
    new Setup(conns2, rigSetups, List())

  /**
   * Create a setup just like this, but the connections replaced at some
   * point in the rig hierarchy
   * @param pos2  The position of the rig to replace
   * @param conns2  The new connections.
   */
  def withConnsReplaced(pos2: List[String], conns2: Set[Connection]): Setup = {
    // Get a setup with a given pos
    def getSetup(p: List[String], s: Setup): Setup = p match {
      case Nil => s
      case p1 :: tail => getSetup(tail, s.rigSetups(p1))
    }
    val oldSetup = getSetup(pos2, this)
    val newSetup = new Setup(conns2, oldSetup.rigSetups, oldSetup.pos)
    // Replace a rig in the hierarchy of rig maps
    def replaceInRigsMaps(p: List[String], s: Setup): Setup = p match {
      case Nil => newSetup
      case p1 :: tail => {
        val nextSetup = s.rigSetups(p1)
        val updatedSetup = replaceInRigsMaps(tail, nextSetup)
        val updatedMap = s.rigSetups.updated(p1, updatedSetup)
        new Setup(s.conns, updatedMap, s.pos)
      }
    }
    replaceInRigsMaps(pos2, this)
  }

  /**
   * Create a new setup just like this, but with the pos updated.
   */
  def withPosUpdated(posNow: List[String]) =
    new Setup(conns, rigSetups, posNow)

  def canEqual(other: Any): Boolean = (other.isInstanceOf[Setup])

  override def equals(other: Any): Boolean =
    other match {
      case that: Setup => (that canEqual this) &&
        this.conns == that.conns &&
        this.rigSetups == that.rigSetups &&
        this.pos == that.pos
      case _ => false
    }

  override def hashCode: Int =
    41 * (41 + conns.hashCode) + rigSetups.hashCode

}

object Setup {
  /**
   * Make an empty setup
   */
  def apply(): Setup = new Setup(Set())

  /**
   * Produce a simple setup.
   */
  def apply(conns: Set[Connection]): Setup = new Setup(conns)
}
