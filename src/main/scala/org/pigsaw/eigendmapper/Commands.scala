package org.pigsaw.eigendmapper

import Preamble._

import java.io.FileWriter

trait Command {
  /** The command itself, as a string. */
  val command: String

  /** A println function (normally scala.Console.println,
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
  
  /** Create a new bls command
   */
  def bls(index: String): BLs = new BLs(index)
  
  /** Create a new bcat command
   */
  def bcat(agent: String): BCat = new BCat(agent)
}

class HelpCommand extends Command {

  val command = "help"

  def action(args: List[String])(state: Setup, prln: PrintlnFn): Setup = {
    prln("""Commands are:
        |help      Show this message
        |into <rigN>  Go into a rig. E.g. into <rig3>
        |graph [agents|ports]  Dump a gexf format file of all the agent or
        |          port connections
        |show <agentName>   Show the connections into and out of an agent.
        |          The agent name includes angle brackets, e.g. <drummer1>
        |snapshot  Capture the state of all the agents' connections. Will not
        |          go into rigs and snapshot those. You need to do them individually."""
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
    val conns = setup.conns(setup.pos)
    val agentQual = agent.qualified(pos)
    
    // If a port ID is in this rig then it doesn't need to
    // qualified. Otherwise, leave is qualified.
    
    def cleaned(portID: String) =
      if (portID.pos == setup.pos) portID.unqualified
      else portID
      
    val links: Set[(String, String, String)] = for {
      conn <- conns
      val master = conn.master
      val slave = conn.slave
      if (master.agent == agentQual || slave.agent == agentQual)
      val link = if (master.agent == agentQual)
        ("", master.nodeLabel, cleaned(slave))
      else
        (cleaned(master), slave.nodeLabel, "")
    } yield link

    if (links.size == 0)
      prln("No agent called " + agent)
    else {
      val padder = new Padder(links.toSeq.sortBy(_._2), " --> ")
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
    val pos = setup.pos
    val bls = this.bls(pos.index)
    val agents = bls.agents
    
    // Update a setup with the results of bcat on a single agent
    def updateWithBCat(ag: String, s: Setup): Setup = {
      val bcat = this.bcat(ag.qualified(pos)) returnedAfter { bc => prln("Examining " + bc.agent) }
      s.withPortNames(pos, ag, bcat.nodeIDNames).
        withConns(pos, bcat.connections)
    }
    
    // Update a setup for a bcat on all agents
    def updateForAgents(ags: List[String], s: Setup): Setup = ags match {
      case Nil => s
      case ag :: tail => updateForAgents(tail, updateWithBCat(ag, s))
    }
    
    val baseSetup = setup.withPortNamesReplaced(pos, Map()).withConnsReplaced(pos, Set())
    updateForAgents(agents, baseSetup)
  }
}

class GraphCommand extends Command {

  val command = "graph"

  def action(args: List[String])(setup: Setup, prln: PrintlnFn): Setup = {
    args match {
      case Nil            => prln("graph: You need to specify something to graph")
      case List("ports")  => doGraph("ports", setup, prln)
      case List("agents") => doGraph("agents", setup, prln)
      case List(_)        => prln("graph: Do not recognise what to graph")
      case _              => prln("graph: Too many arguments")
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
      case "ports"  => setup.ports foreach { out write _.portNodeXML + "\n" }
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
    else
      { prln("No such rig: " + rig); setup }
  }

}