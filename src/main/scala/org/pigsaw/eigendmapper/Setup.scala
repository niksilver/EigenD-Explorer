package org.pigsaw.eigendmapper

import java.util.regex.Pattern

/**
 * A particular set of connections
 * @param conns0  The set of connections in this setup.
 * @param rigSetups0  Mapping from each rig name to its setup.
 */
class Setup(conns0: Set[Connection], rigSetups0: Map[String, Setup]) {

  /**
   * Port connections
   */
  val conns: Set[Connection] = conns0
  /**
   * The rigs setups inside this one.
   * Each one is mapped from its name, such as &lt;rig2&gt;.
   */
  val rigSetups: Map[String, Setup] = rigSetups0

  /**
   * A setup with no internal rig setups
   */
  def this(conns0: Set[Connection]) = this(conns0, Map())

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
  def unified: Setup = {
    val ports = conns flatMap { c => List(c.master, c.slave) }
    val namingPorts = ports filter (_.name.nonEmpty)
    val names: Map[String, String] = namingPorts map { p => (p.id -> p.name.get) } toMap

    // Produce an updated version of the port, with names filled in if available.
    def updated(port: Port): Port = {
      if (port.name.nonEmpty) port
      else Port(port.id, names.get(port.id))
    }

    val updatedConns = conns map (c => Connection(updated(c.master), updated(c.slave)))
    new Setup(updatedConns)
  }

  /**
   * Make a normalised version of this set of connections, in which
   * every port of the form ID &lt;main:agentnameN&gt; is changed to
   * its shorter form of &lt;agentnameN&gt;.
   */
  def normalised: Setup =
    new Setup(conns map { _.normalised })

  /**
   * The rigs in this setup. E.g. <code>"&lt;rig2;&gt"</code>.
   */
  def rigs: Set[String] =
    agents filter { Pattern.matches("<rig\\d+>", _) }

  /**
   * Create a setup just like this, but with a rig setup inside.
   */
  def withRig(rig: String, setup: Setup): Setup =
    new Setup(conns, rigSetups + (rig -> setup))

  def canEqual(other: Any): Boolean = (other.isInstanceOf[Setup])

  override def equals(other: Any): Boolean =
    other match {
      case that: Setup => (that canEqual this) &&
        this.conns == that.conns &&
        this.rigSetups == that.rigSetups
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
   * Produce a normalised, unified setup.
   */
  def apply(conns: Set[Connection]): Setup = new Setup(conns).normalised.unified
}

/**
 * A setup with the additional feature of knowing where in the
 * rig hierarchy we are.
 * @param conns0  The set of connections in this setup.
 * @param rigSetups0  Mapping from each rig name to its setup.
 * @param pos  Our current location in the rig hierarchy, as navigated
 *     by rig names from the top. Empty list means we're at the top setup.
 */
class SetupWithPos(conns0: Set[Connection],
  rigSetups0: Map[String, Setup],
  val pos: List[String]) extends Setup(conns0, rigSetups0) {

  /**
   * Create a SetupWithPos from a Setup, and a given position
   */
  def this(setup: Setup, pos: List[String]) =
    this(setup.conns, setup.rigSetups, pos)
    
  override def canEqual(other: Any): Boolean = other.isInstanceOf[SetupWithPos]
  
  override def equals(other: Any) =
    other match {
    case that: SetupWithPos => super.equals(that) && pos.equals(that.pos)
  }
  
  override def hashCode: Int =
    41 * (41 + super.hashCode) + pos.hashCode
}

object SetupWithPos {
  /**
   * An empty setup.
   */
  def apply(): SetupWithPos = new SetupWithPos(Set(), Map(), List())

  /**
   * Produce a normalised, unified setup with the position at the top.
   */
  def apply(conns: Set[Connection]): SetupWithPos =
    new SetupWithPos(Setup(conns), List())
}