package org.pigsaw.eigendmapper

import java.io.FileWriter

trait Command {
  /** The command itself, as a string. */
  val command: String

  /**
   * The action to perform when the command is found.
   * @param setup  The setup that the command has to act on.
   * @param args   The arguments the user gave after the command
   * @returns  The new setup, after the command has been executed.
   */
  def action(args: List[String])(setup: Setup)(implicit pr: Printer): Setup
  
}
  
  /**
   * Something that allows a println function (fn)
   * @param fn  The println function
   */
  class Printer(val fn: Any => Unit)
  object Printer {
	  implicit val fn: Printer = new Printer(scala.Console.println)
  }

object HelpCommand extends Command {

  val command = "help"

  def action(args: List[String])(state: Setup)(implicit pr: Printer): Setup = {
    pr.fn("""Commands are:
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
object ShowCommand extends Command {

  val command = "show"

  /**
   * The show action. Should have just one argument, which is the
   * name of the agent to show.
   */
  def action(args: List[String])(setup: Setup)(implicit pr: Printer): Setup = {
    args.length match {
      case 0 => pr.fn("show: No agent name given")
      case 1 => doShow(args(0), setup, pr)
      case _ => pr.fn("show: Too many arguments, only one required")
    }

    setup
  }

  def doShow(agent: String, setup: Setup, pr: Printer) {
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
      pr.fn("No agent called " + agent)
    else {
      val padder = new Padder(links.toSeq.sortBy(_._2), " --> ")
      padder.output foreach { pr.fn(_) }
    }
  }
}

/**
 * Capture all the connections in a single setup
 */
object SnapshotCommand extends Command {

  val command = "snapshot"

  def action(args: List[String])(setup: Setup)(implicit pr: Printer): Setup = {
    doSnapshot(pr)
  }
  
  def doSnapshot(pr: Printer): Setup = {
    val bls = new BLs("<main>")
    val agents = bls.agents
    val allConnections = for {
      agent <- agents
      bcat = new BCat(agent)
      conn <- bcat.connections
    } yield { println("Agent " + agent + ", connection " + conn); conn }
    val setup = Setup(allConnections.toSet)
    setup.conns foreach { c => println("Unified: " + c) }
    setup
  }
}

object GraphCommand extends Command {

  val command = "graph"

  def action(args: List[String])(setup: Setup)(implicit pr: Printer): Setup = {
    args match {
      case Nil            => pr.fn("graph: You need to specify something to graph")
      case List("ports")  => doGraph("ports", setup, pr)
      case List("agents") => doGraph("agents", setup, pr)
      case List(x)        => pr.fn("graph: Do not recognise what to graph")
      case _              => pr.fn("graph: Too many arguments")
    }
    setup
  }
  /**
   * Output the gexf file.
   * @param gtype  Whether the graph should be ports (with their agents) or just agents
   * @param conns  The port connections (the state)
   */
  def doGraph(arg: String, state: Setup, pr: Printer) {
    import Graphable._

    val filename = "C:\\cygwin\\home\\Nik\\graph\\output.gexf"
    val out = new FileWriter(filename)
    out write Graphable.gexfHeader

    val localConns = state.agentPortConnections
    val agentConns = state.agentAgentConnections

    out write "<nodes>\n"

    // Declare the agent nodes
    localConns foreach { out write _._1.nodeXML + "\n" }

    // Maybe declare the port nodes
    arg match {
      case "ports"  => state.ports foreach { out write _.nodeXML + "\n" }
      case "agents" => ; // Do nothing
    }

    out write "</nodes>\n"

    out write "<edges>\n"

    command match {
      // When graphing ports: Write port-port edges and agent-port edges
      case "ports" => {
        state.conns foreach { out write _.edgeXML + "\n" }
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

    pr.fn("Output to " + filename)
  }
  
}