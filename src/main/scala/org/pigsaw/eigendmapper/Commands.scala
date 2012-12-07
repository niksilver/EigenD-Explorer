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
  
  /**
   * Something that allows a println function (fn)
   * @param fn  The println function
   */
  class Printer(val fn: Any => Unit)
  object Printer {
	  implicit val fn: Printer = new Printer(scala.Console.println)
  }
  
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