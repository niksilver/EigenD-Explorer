package org.pigsaw.eigendmapper

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

class HelpCommand extends Command {

  val command = "help"

  def action(args: List[String])(state: Setup, prln: PrintlnFn): Setup = {
    prln("""Commands are:
        |dump      Dump the information known about the setup.
        |help      Show this message
        |into <rigN>  Go into a rig. E.g. into <rig3>
        |graph [agents|ports]  Dump a gexf format file of all the agent or
        |          port connections
        |show <agentName>   Show the connections into and out of an agent.
        |          The agent name includes angle brackets, e.g. <drummer1>
        |snapshot  Capture the state of all the agents' connections. Will not
        |          go into rigs and snapshot those. You need to do them individually.
        |up        Go up a level, out of this rig."""
      .stripMargin)
    state
  }

}

/**
 * Show an agent's connections.
 */
class ShowCommand extends Command {

  val command = "show"

  /**
   * The show action. Should have just one argument, which is the
   * name of the agent to show.
   */
  def action(args: List[String])(setup: Setup, prln: PrintlnFn): Setup = {
    args.length match {
      case 0 => prln("show: No agent name given")
      case 1 => doShow(args(0), setup, prln)
      case _ => prln("show: Too many arguments, only one required")
    }

    setup
  }

  def doShow(agent: String, setup: Setup, prln: PrintlnFn) {
    val pos = setup.pos
    val agentQual = agent.defaultQualifier(pos)

    // If a port ID is in this rig then it doesn't need to
    // qualified. Otherwise, leave is qualified.

    def cleaned(portID: String) =
      if (portID.pos == setup.pos) portID.unqualified
      else portID

    // Map a port ID to its best form, meaning its node ID becomes
    // a name if there is one available.

    def bestForm(portID: String): String =
      setup.portIDNamed(portID)

    // Format the columns for a connection between this agent
    // and another (either way round)
    // Returns a this agent's port (without agent name) and
    // the other agent's port (with its name)

//    def format(currAgentPort: String, otherAgentPort: String): (String, String) = {
//      val optSetting = setup.allSettings.get(currAgentPort)
//      val currBest = bestForm(currAgentPort).nodeLabelWithHash
//      val otherBest = cleaned(bestForm(otherAgentPort))
//      val currBestPlusSetting = optSetting match {
//        case Some(value) => currBest + " = " + value
//        case None => currBest
//      }
//      (currBestPlusSetting, otherBest)
//    }
    
    val linksFrom =
      setup.allConns filter { c => c.master.agent == agentQual } map { c => ("", c.master, c.slave) }
    
    val linksTo =
      setup.allConns filter { c => c.slave.agent == agentQual } map { c => (c.master, c.slave, "") }
    
    val linksAll = linksFrom ++ linksTo
    
    def isLinked(portID: String) =
      linksAll exists { _._2 == portID }
    
    val settings = setup.allSettings filterNot { kv => isLinked(kv._1) } map { kv => ("", kv._1, "")}
    
    def formatAgent(portID: String) = {
      val optSetting = setup.allSettings.get(portID)
      val agentBest = bestForm(portID).nodeLabelWithHash
      optSetting match {
        case Some(value) => agentBest + " = " + value
        case None => agentBest
      }
    }
    
    def formatOther(str: String) =
      if (str == "") ""
      else cleaned(bestForm(str))
    
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
      val agQual = ag.qualified(pos)
      val bcat = this.bcat(agQual) returnedAfter { bc => prln("Examining " + bc.agent) }
      val portNames = bcat.nodeIDNames map { pn => (agQual + "#" + pn._1, agQual + " " + pn._2) }
      s.withPortNames(portNames).
        withConns(bcat.connections)
    }

    // Update a setup for a bcat on all agents
    def updateForAgents(ags: List[String], s: Setup): Setup = ags match {
      case Nil => s
      case ag :: tail => updateForAgents(tail, updateWithBCat(ag, s))
    }

    val portIsAtPos = { portID: String => portID.hasPos(pos) }
    val connIsAtPos = { conn: Connection => conn.hasPos(pos) }
    val baseSetup = setup.
      withPortNamesRemoved(portIsAtPos).
      withConnsRemoved(connIsAtPos)
    updateForAgents(agents, baseSetup)
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

    val filename = "C:\\cygwin\\home\\Nik\\graph\\output.gexf"
    val out = new FileWriter(filename)
    out write Graphable.gexfHeader

    val localConns = setup.agentPortConnections
    val agentConns = setup.agentAgentConnections

    out write "<nodes>\n"

    // Declare the agent nodes
    localConns foreach { out write _._1.stringNodeXML + "\n" }

    // Maybe declare the port nodes
    arg match {
      case "ports" => setup.ports foreach { out write _.portNodeXML + "\n" }
      case "agents" => ; // Do nothing
    }

    out write "</nodes>\n"

    out write "<edges>\n"

    command match {
      // When graphing ports: Write port-port edges and agent-port edges
      case "ports" => {
        setup.conns foreach { out write _.edgeXML + "\n" }
        localConns foreach { out write GAgentPort(_).edgeXML + "\n" }
      }
      // When graphing agents: Write agent-agent-edges
      case "agents" => {
        agentConns foreach { out write GAgentAgent(_).edgeXML + "\n" }
      }
    }
    out write "</edges>\n"

    out write Graphable.gexfFooter
    out.close

    prln("Output to " + filename)
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
      case 1 => doInto(args(0), setup, prln)
      case _ => prln("into: Too many arguments"); setup
    }
    prln("Position: " + setup2.pos.displayString)
    setup2
  }

  def doInto(rig: String, setup: Setup, prln: PrintlnFn): Setup = {
    val pos = setup.pos
    val targetPos = pos :+ rig
    val rigQual = rig.qualified(pos)
    if (setup.agents(pos) contains rigQual)
      setup.withPosUpdated(targetPos)
    else { prln("No such rig: " + rig); setup }
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
    if (setup.pos.isEmpty) { prln("Already at top level"); setup }
    else
      setup.withPosUpdated(setup.pos.init)
  }

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
    setup.allPortNames.toSeq.sortBy(_._1).
      foreach { pn => prln(pn._1 + " -> " + pn._2) }

    prln("\nSettings:\n")
    setup.allSettings.toSeq.sortBy(_._1).
      foreach { s => prln(s._1 + " -> " + s._2) }

    prln("\nConnections:\n")
    setup.allConns.toSeq.sortBy(_.master).
      foreach { pn => prln(pn.master + " -> " + pn.slave) }

    prln("\nPos: " + setup.pos.displayString)
    setup
  }
}
