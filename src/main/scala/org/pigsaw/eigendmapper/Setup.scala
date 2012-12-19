package org.pigsaw.eigendmapper

import java.util.regex.Pattern

import Preamble._

/**
 * A particular set of connections
 * @param portNames0  A map from each agent name (unqualified)
 *     to a map from each node ID to its cname (if any).
 *     The we can map from `<cycler1>` to `3.4` to `beat input`.
 * @param conns0  The set of connections in this setup.
 * @param rigSetups0  The rigs setups inside this one.
 *     Each one is mapped from its name, such as &lt;rig2&gt;.
 * @param pos  The position in the rig hierarchy in which we're currently
 *     interested. An empty list means the top level; List("&lt;rig3&gt")
 *     means rig3 within the top level, and so on.
 */
class Setup private(val portNames: Map[String, Map[String, String]],
    conns0: Set[Connection],
    rigSetups0: Map[String, Setup],
    val pos: List[String]) {

  
  /**
   * The connection in this setup, with agent names
   * which are not qualified, and using the best names
   * for ports
   */
  lazy val conns: Set[Connection] =
    bestNames(unqualified(conns0)) filter { _.hasPos(List())}

  lazy val rigSetups: Map[String, Setup] = {
    rigs map { name =>
      rigSetups0.get(name) match {
        case Some(setup) => (name -> setup)
        case None => (name -> Setup())
      }
    } toMap
  }

  /**
   * A setup with no internal rig setups
   */
  def this(conns: Set[Connection]) = this(Map(), conns, Map(), List())

  /**
   * Get all the agent names mentioned in the set of connections,
   * including the angle brackets.
   */
  lazy val agents: Set[String] =
    conns flatMap { _.agents }

  /**
   * Get all the ports named in the connections.
   */
  lazy val ports: Set[String] =
    conns flatMap { c => List(c.master, c.slave) }

  /**
   * Get a map from each agent to each agent (strings, including
   * angle brackets.)
   */
  lazy val agentAgentConnections: Set[(String, String)] =
    conns map { c => (c.master.agent, c.slave.agent) }

  /**
   * Get a map from each agent (a string including angle brackets)
   * to all its ports.
   */
  lazy val agentPortConnections: Set[(String, String)] =
    ports map { p: String => (p.agent -> p) }

  /**
   * Create a unified set of connections. This means if any connections
   * carry a port name, then those names are applied wherever those
   * ports are used.
   */
  private def bestNames(cos: Set[Connection]): Set[Connection] = {
    def bestNameMap(agent: String): Map[String, String] =
      portNames.getOrElse(agent, Map())
    def bestForm(portID: String): String = {
      val bestForms = bestNameMap(portID.agent)
      portID.bestForm(bestForms)
    }
    cos map { c => Connection(bestForm(c.master), bestForm(c.slave)) }
  }

  /**
   * Make an unqualified version of a set of connections, in which
   * every port of the form ID &lt;main:agentnameN&gt; is changed to
   * its shorter form of &lt;agentnameN&gt;.
   */
  private def unqualified(c: Set[Connection]): Set[Connection] =
    c map { _.unqualified }

  /**
   * The rigs in this setup. E.g. <code>"&lt;rig2;&gt"</code>.
   */
  def rigs: Set[String] =
    agents filter { Pattern.matches("<rig\\d+>", _) }

  /**
   * Get the setup in the hierarchy given by the given position.
   */
  def setupForPos(pos: List[String]): Option[Setup] = pos match {
    case Nil => Some(this)
    case rig :: tail => rigSetups.get(rig) match {
      case None => None
      case Some(s) => s.setupForPos(tail)
    }
  }

  /**
   * Create a setup just like this, but with a rig setup inside.
   */
  def withRig(rig: String, setup: Setup): Setup =
    new Setup(portNames, conns, rigSetups + (rig -> setup), pos)

  /**
   * Create a setup just like this, but the with the agent/nodeID/port name
   * map replaced at some point in the rig hierarchy
   * @param pos2  The position of the rig to replace
   * @param portNames2  The new mapping
   */
  def withPortNamesReplaced(pos2: List[String], portNames2: Map[String, Map[String, String]]): Setup = {
    val oldSetup = getSetup(pos2, this)
    val newSetup = new Setup(portNames2, oldSetup.conns, oldSetup.rigSetups, oldSetup.pos)
    replaceInRigsMaps(pos2, newSetup, this)
  }

  /**
   * Create a setup just like this, but  at some point in the rig hierarchy
   * with additional port node IDs for a particular agent.
   * @param pos2  The position of the rig to replace
   * @param agent  The agent name whose node IDs we have names for
   * @param map  The map from node IDs to port names for the agent
   */
  def withPortNames(pos2: List[String], agent: String, map: Map[String, String]): Setup = {
    val oldSetup = getSetup(pos2, this)
    val newPortNames = oldSetup.portNames ++ Map(agent -> map)
    withPortNamesReplaced(pos2, newPortNames)
  }

  /**
   * Create a setup just like this, but with an additional mapping of
   * port names for an agent.
   * @param agent  The agent whose port names we have.
   * @param map  A mapping from each node ID in the agent (e.g. `"15.6.5"`)
   *     to its port name (e.g. `"beat bar input"`).
   */
  def withPortNames(agent: String, map: Map[String, String]): Setup =
    new Setup(portNames ++ Map(agent -> map), conns, rigSetups, pos)

  /**
   *  Get a setup with a given pos. Throw an exception
   *  if a bad post is given
   */
  private def getSetup(p: List[String], s: Setup): Setup = p match {
    case Nil => s
    case p1 :: tail => getSetup(tail, s.rigSetups(p1))
  }

  /**
   *  Replace a rig in the hierarchy of rig maps
   *  @param p  Pos of setup to be replaced
   *  @param newSetup  The new setup
   *  @param s  Current setup
   */
  private def replaceInRigsMaps(p: List[String], newSetup: Setup, s: Setup): Setup = p match {
    case Nil => newSetup
    case p1 :: tail => {
      val nextSetup = s.rigSetups(p1)
      val updatedSetup = replaceInRigsMaps(tail, newSetup, nextSetup)
      val updatedMap = s.rigSetups.updated(p1, updatedSetup)
      new Setup(s.portNames, s.conns, updatedMap, s.pos)
    }
  }

  /**
   * Create a setup just like this, but the connections replaced at some
   * point in the rig hierarchy
   * @param pos2  The position of the rig to replace
   * @param conns2  The new connections.
   */
  def withConnsReplaced(pos2: List[String], conns2: Set[Connection]): Setup = {
    val oldSetup = getSetup(pos2, this)
    val newSetup = new Setup(oldSetup.portNames, conns2, oldSetup.rigSetups, oldSetup.pos)
    replaceInRigsMaps(pos2, newSetup, this)
  }

  /**
   * Create a setup just like this, but with additional connections at some
   * point in the rig hierarchy
   * @param pos2  The position of the rig to replace
   * @param conns2  The extra connections.
   */
  def withConns(pos2: List[String], conns2: Set[Connection]): Setup = {
    val oldSetup = getSetup(pos2, this)
    val newConns = oldSetup.conns ++ conns2
    withConnsReplaced(pos2, newConns)
  }
  
  /**
   * Create a setup just like this, but the with connections replaced.
   * @param conns2  The new connections.
   */
  def withConnsReplaced(conns2: Set[Connection]): Setup =
    withConnsReplaced(List(), conns2)

  /**
   * Create a new setup just like this, but with the pos updated.
   */
  def withPosUpdated(posNow: List[String]) =
    new Setup(portNames, conns, rigSetups, posNow)

  def canEqual(other: Any): Boolean = (other.isInstanceOf[Setup])

  override def equals(other: Any): Boolean =
    other match {
      case that: Setup => (that canEqual this) &&
        this.portNames == that.portNames &&
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
