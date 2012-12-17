package org.pigsaw.eigendmapper

import java.util.regex.Pattern

import Preamble._

/**
 * A particular set of connections
 * @param cnames  A map from each agent name (unqualified)
 *     to a map from each node ID to its cname (if any).
 *     The we can map from `<cycler1>` to `3.4` to `beat input`. 
 * @param conns0  The set of connections in this setup.
 * @param rigSetups  The rigs setups inside this one.
 *     Each one is mapped from its name, such as &lt;rig2&gt;.
 * @param pos  The position in the rig hierarchy in which we're currently
 *     interested. An empty list means the top level; List("&lt;rig3&gt")
 *     means rig3 within the top level, and so on.
 */
class Setup(val cnames: Map[String, Map[String,String]],
    val conns0: Set[Connection],
    val rigSetups0: Map[String, Setup],
    val pos: List[String]) {

  lazy val conns: Set[Connection] = unified(unqualified(conns0))

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
  private def unified(cos: Set[Connection]): Set[Connection] = {
    def bestNames(agent: String): Map[String, String] =
      cnames.getOrElse(agent, Map())
    def bestForm(portID: String): String = {
      val bestForms = bestNames(portID.agent)
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
      case None    => None
      case Some(s) => s.setupForPos(tail)
    }
  }
  
  /**
   * Create a setup just like this, but with a rig setup inside.
   */
  def withRig(rig: String, setup: Setup): Setup =
    new Setup(cnames, conns, rigSetups + (rig -> setup), pos)

  /**
   * Create a setup just like this, but the with connections replaced.
   * @param conns2  The new connections.
   */
  def withConnsReplaced(conns2: Set[Connection]): Setup =
    new Setup(cnames, conns2, rigSetups, List())

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
    val newSetup = new Setup(oldSetup.cnames, conns2, oldSetup.rigSetups, oldSetup.pos)
    // Replace a rig in the hierarchy of rig maps
    def replaceInRigsMaps(p: List[String], s: Setup): Setup = p match {
      case Nil => newSetup
      case p1 :: tail => {
        val nextSetup = s.rigSetups(p1)
        val updatedSetup = replaceInRigsMaps(tail, nextSetup)
        val updatedMap = s.rigSetups.updated(p1, updatedSetup)
        new Setup(s.cnames, s.conns, updatedMap, s.pos)
      }
    }
    replaceInRigsMaps(pos2, this)
  }

  /**
   * Create a setup just like this, but the cnames replaced at some
   * point in the rig hierarchy
   * @param pos2  The position of the rig to replace
   * @param cnames2  The new cnames.
   */
  def withCNamesReplaced(pos2: List[String], cnames2: Map[String, Map[String,String]]): Setup = {
    // Get a setup with a given pos
    def getSetup(p: List[String], s: Setup): Setup = p match {
      case Nil => s
      case p1 :: tail => getSetup(tail, s.rigSetups(p1))
    }
    val oldSetup = getSetup(pos2, this)
    val newSetup = new Setup(cnames2, oldSetup.conns, oldSetup.rigSetups, oldSetup.pos)
    // Replace a rig in the hierarchy of rig maps
    def replaceInRigsMaps(p: List[String], s: Setup): Setup = p match {
      case Nil => newSetup
      case p1 :: tail => {
        val nextSetup = s.rigSetups(p1)
        val updatedSetup = replaceInRigsMaps(tail, nextSetup)
        val updatedMap = s.rigSetups.updated(p1, updatedSetup)
        new Setup(s.cnames, s.conns, updatedMap, s.pos)
      }
    }
    replaceInRigsMaps(pos2, this)
  }

  /**
   * Create a new setup just like this, but with the pos updated.
   */
  def withPosUpdated(posNow: List[String]) =
    new Setup(cnames, conns, rigSetups, posNow)

  def canEqual(other: Any): Boolean = (other.isInstanceOf[Setup])

  override def equals(other: Any): Boolean =
    other match {
      case that: Setup => (that canEqual this) &&
        this.cnames == that.cnames &&
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
