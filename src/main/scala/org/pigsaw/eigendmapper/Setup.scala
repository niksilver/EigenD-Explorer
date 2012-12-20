package org.pigsaw.eigendmapper

import java.util.regex.Pattern

import Preamble._

/**
 * A particular set of connections
 * @param portNames0  A map from each agent name (unqualified)
 *     to a map from each node ID to its cname (if any).
 *     The we can map from `<cycler1>` to `3.4` to `beat input`.
 * @param conns0  The set of connections in this setup.
 * @param pos  The position in the rig hierarchy in which we're currently
 *     interested. An empty list means the top level; List("&lt;rig3&gt")
 *     means rig3 within the top level, and so on.
 */
class Setup private(private val portNames0: Map[String, Map[String, String]],
    private val conns0: Set[Connection],
    rigSetups0: Map[String, Setup],
    val pos: List[String]) {
  
  /**
   * The port names at a given position,
   * including their qualifiers
   */
  def portNames(p: List[String]): Map[String, Map[String, String]] =
    portNames0 filter { _._1 hasPos p }
  
  /**
   * The port names at the top level
   */
  def portNames: Map[String, Map[String, String]] = portNames(List())

  /**
   * The connections at a given pos, including their qualifiers.
   */
  def conns(p: List[String]): Set[Connection] =
    bestNames(unqualified(conns0)) filter { _ hasPos p }
  
  /**
   * The connections in this setup, with agent names
   * which are not qualified, and using the best names
   * for ports
   */
  lazy val conns: Set[Connection] = conns(List())

  // NOT DONE!
//  lazy val rigSetupsXX: Map[String, Setup] = {
//    rigs map { name =>
//      rigSetups0.get(name) match {
//        case Some(setup) => (name -> setup)
//        case None => (name -> Setup())
//      }
//    } toMap
//  }

  /**
   * A setup with no internal rig setups
   */
  def this(conns: Set[Connection]) = this(Map(), conns, Map(), List())

  /**
   * Get all the agents at a particular position.
   */
  def agents(p: List[String]): Set[String] =
    conns(p) flatMap { _.agents }
  
  /**
   * Get all the agent mentioned in the top level set of connections,
   * including the angle brackets.
   */
  lazy val agents: Set[String] = agents(List())

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
   * The rigs in this setup. E.g. `"<rig2>"`.
   */
  def rigs: Set[String] =
    agents filter { Pattern.matches("<rig\\d+>", _) }

  /**
   * Get the setup in the hierarchy given by the given position.
   */
  // NOT DONE
//  def setupForPosXX(pos: List[String]): Option[Setup] = pos match {
//    case Nil => Some(this)
//    case rig :: tail => rigSetups.get(rig) match {
//      case None => None
//      case Some(s) => s.setupForPos(tail)
//    }
//  }

  /**
   * Create a setup just like this, but with a rig setup inside.
   */
  // NOT DONE!!
//  def withRigXX(rig: String, setup: Setup): Setup =
//    new Setup(portNames, conns, rigSetups + (rig -> setup), pos)

  /**
   * Create a setup just like this, but the with the agent/nodeID/port name
   * map replaced at some point in the rig hierarchy
   * @param pos2  The position of the rig to replace
   * @param portNames2  The new mapping
   */
  def withPortNamesReplaced(pos2: List[String], portNames2: Map[String, Map[String, String]]): Setup = {
    val portNamesCleaned = portNames0 filterNot { _._1.hasPos(pos2) }
    new Setup(portNamesCleaned ++ portNames2, this.conns0, Map(), this.pos)
  }

  /**
   * Create a setup just like this, but  at some point in the rig hierarchy
   * with additional port node IDs for a particular agent.
   * @param pos2  The position of the rig to replace
   * @param agent  The agent name whose node IDs we have names for
   * @param map  The map from node IDs to port names for the agent
   */
  def withPortNames(pos2: List[String], agent: String, map: Map[String, String]): Setup = {
    new Setup(portNames0 ++ Map(agent.qualified(pos2) -> map), this.conns0, Map(), this.pos)
  }

  /**
   * Create a setup just like this, but with an additional mapping of
   * port names for an agent.
   * @param agent  The agent whose port names we have.
   * @param map  A mapping from each node ID in the agent (e.g. `"15.6.5"`)
   *     to its port name (e.g. `"beat bar input"`).
   */
  def withPortNames(agent: String, map: Map[String, String]): Setup =
    withPortNames(List(), agent, map)

  /**
   *  Get a setup with a given pos. Throw an exception
   *  if a bad post is given
   */
  // NOT DONE
//  private def getSetupXX(p: List[String], s: Setup): Setup = p match {
//    case Nil => s
//    case p1 :: tail => getSetup(tail, s.rigSetups(p1))
//  }

  /**
   *  Replace a rig in the hierarchy of rig maps
   *  @param p  Pos of setup to be replaced
   *  @param newSetup  The new setup
   *  @param s  Current setup
   */
  // NOT DONE
//  private def replaceInRigsMapsXX(p: List[String], newSetup: Setup, s: Setup): Setup = p match {
//    case Nil => newSetup
//    case p1 :: tail => {
//      val nextSetup = s.rigSetups(p1)
//      val updatedSetup = replaceInRigsMaps(tail, newSetup, nextSetup)
//      val updatedMap = s.rigSetups.updated(p1, updatedSetup)
//      new Setup(s.portNames, s.conns, updatedMap, s.pos)
//    }
//  }

  /**
   * Create a setup just like this, but the connections replaced at some
   * point in the rig hierarchy
   * @param pos2  The position of the rig to replace
   * @param conns2  The new connections.
   */
  def withConnsReplaced(pos2: List[String], conns2: Set[Connection]): Setup = {
    val connsCleaned = conns0 filterNot { _.hasPos(pos2) }
    new Setup(portNames0, connsCleaned ++ conns2, Map(), pos)
  }

  /**
   * Create a setup just like this, but with additional connections at some
   * point in the rig hierarchy
   * NB: Currently not clear if the conns2 are qualified.
   * @param pos2  The position of the rig to replace
   * @param conns2  The extra connections.
   */
  def withConns(pos2: List[String], conns2: Set[Connection]): Setup = {
    new Setup(portNames0, conns0 ++ conns2, Map(), pos)
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
    new Setup(portNames0, conns0, Map(), posNow)

  def canEqual(other: Any): Boolean = (other.isInstanceOf[Setup])

  override def equals(other: Any): Boolean =
    other match {
      case that: Setup => (that canEqual this) &&
        this.portNames0 == that.portNames0 &&
        this.conns0 == that.conns0 &&
        this.pos == that.pos
      case _ => false
    }

  override def hashCode: Int =
    41 * (41 * (41 + portNames0.hashCode) + conns0.hashCode) + pos.hashCode

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
