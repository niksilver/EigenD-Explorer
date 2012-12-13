package org.pigsaw.eigendmapper

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
   * @returns  The new setup, after the command has been executed.
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
        |graph [agents|ports]  Dump a gexf format file of all the agent or
        |          port connections
        |show <agentName>   Show the connections into and out of an agent.
        |          The agent name includes angle brackets, e.g. <drummer1>
        |snapshot  Capture the state of all the agents' connections"""
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
    val links: Set[(String, String, String)] = for {
      conn <- setup.conns
      val master = conn.master
      val slave = conn.slave
      if (master.agent == Some(agent) || slave.agent == Some(agent))
      val link = if (master.agent == Some(agent))
        ("", master.nonEmptyName, slave.nonEmptyFQName)
      else
        (master.nonEmptyFQName, slave.nonEmptyName, "")
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
  
  /**
   * Convert a pos to an index specification for the bls command:
   * List() =>         <main>
   * List("<rig1>") => <main.rig1:main>
   * List("<rig2>") => <main.rig1:main.rig2:main>
   */
  def index(pos: List[String]): String = {
    def strip(name: String): String = ("<(.*)>".r findFirstMatchIn name).get.group(1)
    def insertMains(s: String) = "." + strip(s) + ":main"
    "<main" + (pos map insertMains).mkString + ">"
  }
  
  def doSnapshot(setup: Setup, prln: PrintlnFn): Setup = {
    val index = this.index(setup.pos)
    val bls = this.bls(index)
    val agents = bls.agents
    val allConnections = for {
      agent <- agents
      bcat = this.bcat(agent)
      conn <- bcat.connections
    } yield { prln("Agent " + agent + ", connection " + conn); conn }
    setup.withConnsReplaced(setup.pos, allConnections.toSet)
  }
}

class GraphCommand extends Command {

  val command = "graph"

  def action(args: List[String])(setup: Setup, prln: PrintlnFn): Setup = {
    args match {
      case Nil            => prln("graph: You need to specify something to graph")
      case List("ports")  => doGraph("ports", setup, prln)
      case List("agents") => doGraph("agents", setup, prln)
      case List(x)        => prln("graph: Do not recognise what to graph")
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
    localConns foreach { out write _._1.nodeXML + "\n" }

    // Maybe declare the port nodes
    arg match {
      case "ports"  => setup.ports foreach { out write _.nodeXML + "\n" }
      case "agents" => ; // Do nothing
    }

    out write "</nodes>\n"

    out write "<edges>\n"

    command match {
      // When graphing ports: Write port-port edges and agent-port edges
      case "ports" => {
        setup.conns foreach { out write _.edgeXML + "\n" }
        localConns foreach { out write _.edgeXML + "\n" }
      }
      // When graphing agents: Write agent-agent-edges
      case "agents" => {
        agentConns foreach { out write _.edgeXML + "\n" }
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

  def action(args: List[String])(setup: Setup, prln: PrintlnFn): Setup =
    setup.withPosUpdated(setup.pos :+ args(0))

}