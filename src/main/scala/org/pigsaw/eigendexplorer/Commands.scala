/*
 *  Copyright 2012 Nik Silver.
 *  
 *  This file is part of EigenD Explorer.
 *
 *  EigenD Explorer is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  EigenD Explorer is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with EigenD Explorer.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.pigsaw.eigendexplorer

import Preamble._

import java.io.FileWriter

trait Command {
  /** The command itself, as a string. */
  val command: String

  /**
   * A println function (normally scala.Console.println,
   *     but can be changed for testing purposes).
   */
  type PrintlnFn = Any => Unit

  /**
   * The action to perform when the command is found.
   * @param args   The arguments the user gave after the command
   * @param setup  The setup that the command has to act on.
   * @param prln   The println function to use
   * @return  The new setup, after the command has been executed.
   */
  def action(args: List[String])(setup: Setup, prln: PrintlnFn): Setup

  /**
   * Create a new bls command
   */
  def bls(index: String): BLs = new BLs(index)

  /**
   * Create a new bcat command
   */
  def bcat(agent: String): BCat = new BCat(agent)
}

/**
 * Dump the current setup
 */
class DumpCommand extends Command {

  val command = "dump"

  /**
   * The dump action. Dump all the information about the current setup,
   * in a pretty raw form.
   */
  def action(args: List[String])(setup: Setup, prln: PrintlnFn): Setup = {
    prln("Port names:\n")
    setup.allPortNames.toSeq.sortBy(_._1.toString).
      foreach { pn => prln(pn._1 + " -> " + pn._2) }

    prln("\nSettings:\n")
    setup.allSettings.toSeq.sortBy(_._1.toString).
      foreach { s => prln(s._1 + " = " + s._2) }

    prln("\nConnections:\n")
    setup.allConns.toSeq.sortBy(_.master.toString).
      foreach { pn => prln(pn.master + " -> " + pn.slave) }

    prln("\nPosition: " + setup.pos.displayString)
    setup
  }
}

class GraphCommand extends Command {

  val command = "graph"

  def action(args: List[String])(setup: Setup, prln: PrintlnFn): Setup = {
    args match {
      case Nil => prln("graph: You need to specify something to graph")
      case List("ports") => doGraph("ports", setup, prln)
      case List("agents") => doGraph("agents", setup, prln)
      case List(_) => prln("graph: Do not recognise what to graph")
      case _ => prln("graph: Too many arguments")
    }
    setup
  }

  /**
   * Output the gexf file.
   * @param arg    What to graph, either "agents" or "ports"
   * @param setup  The current setup to graph
   * @param prln   The print output function
   */
  def doGraph(arg: String, setup: Setup, prln: PrintlnFn) {
    import Graphable._

    prln(Graphable.gexfHeader)

    val ppConns = portPortConns(setup)
    val apConns = agentPortConns(setup)
    val paConns = portAgentConns(setup)
    val aaConns = agentAgentConns(setup)

    val ports = (apConns map { _._2 }) ++ (paConns map { _._1 })
    val agents = aaConns flatMap { aa => List(aa._1, aa._2) }

    prln("<nodes>")

    // Declare the agent nodes
    agents foreach { a => prln(a.stringNodeXML) }

    // If required declare the port nodes
    arg match {
      case "ports" => ports foreach { p => prln(p.portNodeXML) }
      case "agents" => ; // Do nothing
    }

    prln("</nodes>")

    prln("<edges>")

    arg match {
      // When graphing ports: Write port-port edges,
      // agent-port edges, and port-agent edges
      case "ports" => {
        ppConns foreach { c => prln(GPortPort(c).edgeXML) }
        apConns foreach { ap => prln(GAgentPort(ap).edgeXML) }
        paConns foreach { pa => prln(GPortAgent(pa).edgeXML) }
      }
      // When graphing agents: Write agent-agent edges
      case "agents" => {
        aaConns foreach { aa => prln(GAgentAgent(aa).edgeXML) }
      }
    }
    prln("</edges>")

    prln(Graphable.gexfFooter)
  }

  /**
   * Get connections involving any port at the current pos.
   * The ports will have names if possible, and will be
   * unqualified if at the current pos.
   */
  def portPortConns(s: Setup): Set[Connection] = {
    val p = s.pos
    for {
      c <- s.conns
      master2 = s.portIDNamed(c.master) unqualifiedForPos p
      slave2 = s.portIDNamed(c.slave) unqualifiedForPos p
    } yield Connection(master2, slave2)
  }

  /**
   * Get a map from each agent to its port
   * where the port is part of the port-port mapping at this pos.
   * Agent names (and port IDs) will be unqualified if they are
   * at the current pos.
   */
  def agentPortConns(s: Setup): Set[(Agent, PortID)] =
    portPortConns(s) map { c => (c.master.agent, c.master) }

  /**
   * For every port in the port-port mapping at this pos,
   * get a pair of the slave (end) port and its agent.
   * Agent names (and port IDs) will be unqualified if they are
   * at the current pos.
   */
  def portAgentConns(s: Setup): Set[(PortID, Agent)] =
    portPortConns(s) map { c => (c.slave, c.slave.agent) }

  /**
   * Get pairs of master and slave agent names.
   * Includes only pairs in which one is in the current rig.
   * Agent names in the current rig will be unqualified.
   */
  def agentAgentConns(s: Setup): Set[(Agent, Agent)] =
    portPortConns(s) map { c => (c.master.agent, c.slave.agent) }
}

class HelpCommand extends Command {

  val command = "help"

  def action(args: List[String])(state: Setup, prln: PrintlnFn): Setup = {
    prln("""Commands are:
        |
        |dump
        |        Dump the information known about the setup.
        |
        |graph agents
        |graph ports
        |        Output the agent or port connections in gexf format. Most useful
        |        with file redirection (see below).
        |
        |help
        |        Show this message
        |
        |inspect <agentName>
        |        Inspect an agent's settings and connections. The agent name includes
        |        angle brackets. Example: inspect <drummer1>
        |
        |into <rigN>
        |        Go into a rig. Example: into <rig3>
        |
        |snapshot
        |        Capture the state of all the agents' connections and settings. Will
        |        not go into rigs; you will need to snapshot those individually.
        |
        |up
        |        Go up a level, out of this rig.
        |
        |You can redirect the output of a command to a file by using > filename.ext
        |after it, and you can use quote marks to include spaces. Example:
        |    graph ports > '/home/anna/Factory Setup 1.gexf'""".stripMargin)
    state
  }

}

/**
 * Inspect an agent's connections and settings.
 */
class InspectCommand extends Command {

  val command = "inspect"

  /**
   * The inspect action. Should have just one argument, which is the
   * name of the agent to inspect.
   */
  def action(args: List[String])(setup: Setup, prln: PrintlnFn): Setup = {
    args.length match {
      case 0 => prln("inspect: No agent name given")
      case 1 => if (Agent.isAgent(args(0))) doInspect(Agent(args(0)), setup, prln)
      else prln("Bad agent name. Names should be similar to <clicker1>.")
      case _ => prln("inspect: Too many arguments, only one required")
    }

    setup
  }

  def doInspect(agent: Agent, setup: Setup, prln: PrintlnFn) {
    val pos = setup.pos
    val agentQual = agent.defaultQualifier(pos)

    // If a port ID is in this rig then it doesn't need to
    // qualified. Otherwise, leave is qualified.

    def cleaned(portID: PortID): PortID =
      if (portID.pos == setup.pos) portID.unqualified
      else portID

    // Map a port ID to its best form, meaning its node ID becomes
    // a name if there is one available.

    def bestForm(portID: PortID): PortID =
      setup.portIDNamed(portID)

    val linksFrom: Set[(String, String, String)] =
      setup.allConns filter {
        c => c.master.agent == agentQual } map {
        c => ("", c.master.toString, c.slave.toString) }

    val linksTo: Set[(String, String, String)] =
      setup.allConns filter {
        c => c.slave.agent == agentQual } map {
        c => (c.master.toString, c.slave.toString, "") }

    val linksAll = linksFrom ++ linksTo

    def isLinked(portID: PortID) =
      linksAll exists { _._2 == portID }

    def omitValue(v: String): Boolean =
      (v == "" || (v.startsWith("<") && v.contains(">")))

    def tidyValue(v: String): String = {
      // Truncate decimals at 3dp
      val Decimal = """(-?\d+\.)(\d\d\d)\d*""".r
      val tidy1 = v match {
        case Decimal(pre, post) => pre + post
        case _ => v
      }
      tidy1
    }

    val settings: Set[(String, String, String)] = {
      for {
        (port, v) <- setup.allSettings.toSet
        if port.agent == agentQual
        if !isLinked(port) && !omitValue(v)
      } yield ("", port.toString, "")
    }

    def formatAgent(portID: String): String = {
      val optSetting = setup.allSettings.get(portID)
      val agentBest = bestForm(portID).nodeLabelWithHash
      // Add the setting if it exists and is not the empty string
      optSetting match {
        case Some(value) => agentBest + (if (omitValue(value)) "" else " = " + tidyValue(value))
        case None => agentBest
      }
    }

    def formatOther(str: PortID): String =
      if (str == "") ""
      else bestForm(str).unqualifiedForPos(pos).toString

    val cols = (linksAll ++ settings) map { c => (formatOther(c._1), formatAgent(c._2), formatOther(c._3)) }

    if (cols.size == 0)
      prln("No agent called " + agent)
    else {
      type Cols = Tuple3[String, String, String]
      val sorter = ((a: Cols, b: Cols) => lessThanAlphaInts(a._2, b._2))
      val padder = new Padder(cols.toSeq.sortWith(sorter), " --> ")
      padder.output foreach { prln(_) }
    }
  }
}

/**
 * Go into a rig
 */
class IntoCommand extends Command {

  val command = "into"

  def action(args: List[String])(setup: Setup, prln: PrintlnFn): Setup = {
    val setup2 = args.length match {
      case 0 => prln("into: Too few arguments"); setup
      case 1 => {
        val arg = args(0)
        if (arg.isRig) doInto(Agent(args(0)), setup, prln)
        else if (Agent.isAgent(arg)) { prln("into: " + arg + " is not a rig"); setup }
        else { prln("into: Bad rig name. Should be of the form <rig3>"); setup }
      }
      case _ => prln("into: Too many arguments"); setup
    }
    prln("Position: " + setup2.pos.displayString)
    setup2
  }

  def doInto(rig: Agent, setup: Setup, prln: PrintlnFn): Setup = {
    val pos = setup.pos
    val targetPos = pos :+ rig
    val rigQual = rig.qualified(pos)
    if (setup.agents(pos) contains rigQual)
      setup.withPosUpdated(targetPos)
    else { prln("No such rig: " + rig); setup }
  }

}

/**
 * Capture all the connections in a single setup
 */
class SnapshotCommand extends Command {

  val command = "snapshot"

  def action(args: List[String])(setup: Setup, prln: PrintlnFn): Setup = {
    doSnapshot(setup, prln)
  }

  def doSnapshot(setup: Setup, prln: PrintlnFn): Setup = {
    prln("Starting snapshot...")

    val pos = setup.pos
    val bls = this.bls(pos.index)
    val agents = bls.agents

    // Update a setup with the results of bcat on a single agent
    def updateWithBCat(ag: String, s: Setup): Setup = {
      println("************** Agent is " + ag)
      val agQual = Agent(ag).qualified(pos).toString
      val bcat = this.bcat(agQual) returnedAfter { bc => prln("Examining " + bc.agent) }
      val portNames = bcat.nodeIDNames filter {
        nn => nn._2 != ""
        } map { pn =>
        println("************** " + pn)
        val pnFrom = PortID(agQual + "#" + pn._1)
        val pnTo = PortID(agQual + " " + pn._2)
        (pnFrom, pnTo) }
      s.withPortNames(portNames).
        withSettings(bcat.settings).
        withConns(bcat.connections)
    }

    // Update a setup for a bcat on all agents
    def updateForAgents(ags: List[String], s: Setup): Setup = ags match {
      case Nil => s
      case ag :: tail => updateForAgents(tail, updateWithBCat(ag, s))
    }

    val portIsAtPos = { portID: PortID => portID.hasPos(pos) }
    val connIsAtPos = { conn: Connection => conn.hasPos(pos) }
    val baseSetup = setup.
      withPortNamesRemoved(portIsAtPos).
      withSettingsRemoved(portIsAtPos).
      withConnsRemoved(connIsAtPos)
    updateForAgents(agents, baseSetup)
  }
}

/**
 * Go up a loevel
 */
class UpCommand extends Command {

  val command = "up"

  def action(args: List[String])(setup: Setup, prln: PrintlnFn): Setup = {
    val setup2 = args.length match {
      case 0 => doUp(setup, prln)
      case _ => prln("up: Does not take arguments"); setup
    }
    prln("Position: " + setup2.pos.displayString)
    setup2
  }

  def doUp(setup: Setup, prln: PrintlnFn): Setup = {
    if (setup.pos.topLevel) { prln("Already at top level"); setup }
    else
      setup.withPosUpdated(setup.pos.parent)
  }

}
