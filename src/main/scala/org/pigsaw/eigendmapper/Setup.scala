package org.pigsaw.eigendmapper

import java.util.regex.Pattern

import Preamble._

/**
 * A particular set of connections
 * @param portNames0  A map from a port ID (with a node ID) to the same
 *     port ID but with a name.
 * @param settings0  A map from each port ID to its set value. 
 * @param conns0  The set of connections in this setup. Do not use this
 *     internally; use allConns instead.
 * @param pos  The position in the rig hierarchy in which we're currently
 *     interested. An empty list means the top level; List("&lt;rig3&gt")
 *     means rig3 within the top level, and so on.
 */
class Setup private(private val portNames0: Map[String, String],
    private val settings0: Map[String, String],
    private val conns0: Set[Connection],
    val pos: List[String]) {

  /**
   * The mapping from each port ID (with node ID) to the port ID with
   * a name. All port IDs are fully qualified.
   */
  lazy val allPortNames: Map[String, String] = portNames0
  // Note that it's okay to use portNames0 because whenever it's set
  // it also gets fully qualified, too.

  /**
   * The named version of a port ID, or the port ID itself if there
   * is no name. The given port ID is expected to be fully
   * qualified. The returned port ID is fully qualified.
   */
  def portIDNamed(portID: String): String =
    portNames0.getOrElse(portID, portID)
  
  /**
   * All the settings, with port IDs fully qualified.
   */
  lazy val allSettings: Map[String, String] = settings0
  // Note that it's okay to use portNames0 because whenever it's set
  // it also gets fully qualified, too.
  
  /**
   * Get all the connections, fully qualified.
   */
  lazy val allConns =
    conns0 map { _ defaultQualifier List() }
    
  /**
   * The connections at a given pos, including their qualifiers.
   */
  def conns(p: List[String]): Set[Connection] =
    allConns filter { _ hasPos p }

  /**
   * The connections in this setup at the top level, with agent names
   * which are not qualified, and using the best names
   * for ports
   */
  lazy val conns: Set[Connection] = conns(pos)

  /**
   * A setup with no internal rig setups
   */
  def this(conns: Set[Connection]) = this(Map(), Map(), conns, List())

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
   * Get all the ports named in the connections
   * at the current pos.
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
   * Make an unqualified version of a set of connections, in which
   * every port of the form ID &lt;main:agentnameN&gt; is changed to
   * its shorter form of &lt;agentnameN&gt;.
   */
  private def unqualified(c: Set[Connection]): Set[Connection] =
    c map { _.unqualified }

  /**
   * The rigs in this setup at the current pos, unqualified. E.g. `"<rig2>"`.
   */
  def rigs: Set[String] = rigs(pos)

  /**
   * The rigs in this setup at the given pos.
   */
  def rigs(p: List[String]): Set[String] =
    allConns flatMap { _.agents } flatMap { ag =>
      Set() ++
        (if (ag.pos == p && ag.isRig) Set(ag.unqualified) else Set()) ++
        (if (ag.pos.length >= 1 && ag.pos.init == p) Set(ag.pos.last) else Set())
      
    }

  /**
   * Qualify all unqualified port names in this mapping to the current pos.
   */
  private def defaultPortNamesQualifier(portNames2: Map[String, String]): Map[String, String] =
    portNames2 map { fromTo =>
      (fromTo._1.defaultQualifier(pos), fromTo._2.defaultQualifier(pos)) }
  
  /**
   * Create a setup just like this, but with the map from port ID with node IDs
   * to port IDs with names replaced with the given one. Any port IDs which are
   * unqualified are given the current pos.
   * @param portNames2  The new map from port IDs (with node ID) to port IDs (with names)
   */
  def withPortNamesReplaced(portNames2: Map[String, String]): Setup = {
    val namesQual = defaultPortNamesQualifier(portNames2)
    new Setup(namesQual, settings0, allConns, pos)
  }

  /**
   * Create a setup just like this, but with the specified port ID name mappings
   * removed.
   * @param test  A test for each port ID (with node name), and if true
   *     its mapping is removed.
   */
  def withPortNamesRemoved(test: String => Boolean): Setup = {
    val portNamesCleaned = allPortNames filterNot { pn => test(pn._1) }
    new Setup(portNamesCleaned, settings0, allConns, pos)
  }

  /**
   * Create a setup just like this, but with the
   * additional port names mappings.
   * @param portNames2  The additional mappings from port IDs (with node ID)
   *     to port IDs (with names)
   */
  def withPortNames(portNames2: Map[String, String]): Setup = {
    val namesQual = defaultPortNamesQualifier(portNames2)
    new Setup(portNames0 ++ namesQual, settings0, allConns, pos)
  }

  /**
   * Qualify all unqualified settings in this mapping to the current pos.
   */
  private def defaultSettingsQualifier(settings2: Map[String, String]): Map[String, String] =
    settings2 map { nameValue =>
      (nameValue._1.defaultQualifier(pos), nameValue._2) }
  
  /**
   * Create a setup just like this, but with the new settings given.
   */
  def withSettingsReplaced(settings2: Map[String, String]): Setup = {
    val settingsQual = defaultSettingsQualifier(settings2)
    new Setup(portNames0, settingsQual, allConns, pos)
  }

  /**
   * Create a setup just like this, but with the specified settings
   * removed.
   * @param test  A test for each port ID, and if true
   *     its setting is removed.
   */
  def withSettingsRemoved(test: String => Boolean): Setup = {
    val settingsCleaned = allSettings filterNot { pn => test(pn._1) }
    new Setup(portNames0, settingsCleaned, allConns, pos)
  }

  /**
   * Create a setup just like this, but with the
   * additional settings.
   * @param settings2  The additional settings
   */
  def withSettings(settings2: Map[String, String]): Setup = {
    val settingsQual = defaultSettingsQualifier(settings2)
    new Setup(portNames0, settings0 ++ settingsQual, allConns, pos)
  }

  /**
   * Create a setup just like this, but all the connections replaced.
   * Unqualified port IDs will default to the current pos.
   * @param conns2  The new connections.
   */
  def withConnsReplaced(conns2: Set[Connection]): Setup = {
    val connsQual = conns2 map { _.defaultQualifier(pos) }
    new Setup(portNames0, settings0, connsQual, pos)
  }

  /**
   * Create a setup just like this, but with additional connections.
   * Unqualified port IDs will default to the current pos.
   * @param conns2  The extra connections.
   */
  def withConns(conns2: Set[Connection]): Setup = {
    val connsQual = conns2 map { _.defaultQualifier(pos) }
    new Setup(portNames0, settings0, allConns ++ connsQual, pos)
  }

  /**
   * Create a setup just like this, but with connections that satisfy
   * the given test removed.
   * @param test  The test to see if a connection should be removed.
   */
  def withConnsRemoved(test: Connection => Boolean): Setup = {
    new Setup(portNames0, settings0, allConns filterNot test, pos)
  }

  /**
   * Create a new setup just like this, but with the pos updated.
   */
  def withPosUpdated(posNow: List[String]) =
    new Setup(portNames0, settings0, allConns, posNow)

  def canEqual(other: Any): Boolean = (other.isInstanceOf[Setup])

  override def equals(other: Any): Boolean =
    other match {
      case that: Setup => (that canEqual this) &&
        this.portNames0 == that.portNames0 &&
        this.settings0 == that.settings0 &&
        this.allConns == that.allConns &&
        this.pos == that.pos
      case _ => false
    }

  override def hashCode: Int =
    41 * (41 * (41 * (41 + portNames0.hashCode) + settings0.hashCode) + allConns.hashCode) + pos.hashCode

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
