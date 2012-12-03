package org.pigsaw.eigendmapper

import java.io.FileWriter
import scala.util.parsing.combinator.RegexParsers

object Console {

  type Conns = Set[Connection]

  def main(args: Array[String]) {
    val parser = new ConsoleParser
    val ul = new UserLine(">> ")
    actLoop(Set())

    def actLoop(state: Conns): Unit = {
      ul.line match {
        case None => ; // Do nothing
        case Some(input) => {
          val state2 = act(input, state)
          actLoop(state2)
        }
      }
    }

    def act(line: String, state: Conns): Conns = {
      parser.parseLine(line) match {
        case Some(Snapshot) => snapshot
        case Some(GraphPorts) => writeGraph(GraphPorts)(state); state
        case Some(GraphAgents) => writeGraph(GraphAgents)(state); state
        case Some(Show(agent)) => show(agent, state); state
        case Some(Help) => help; state
        case None => println("Unknown command"); state
      }
    }
  }

  sealed abstract class Command
  object Snapshot extends Command
  abstract class GraphCommand extends Command
  object GraphPorts extends GraphCommand
  object GraphAgents extends GraphCommand
  case class Show(agent: String) extends Command
  object Help extends Command

  class ConsoleParser extends RegexParsers {
    override type Elem = Char

    def command = snapshot | graphPorts | graphAgents | show | help

    def snapshot = "snapshot" ^^ { _ => Snapshot }
    def graphPorts = "graph" ~ "ports" ^^ { _ => GraphPorts }
    def graphAgents = "graph" ~ "agents" ^^ { _ => GraphAgents }
    def show = "show" ~> agent ^^ { Show(_) }
    def agent = """\S+""".r
    def help = "help" ^^ { _ => Help }

    def parseLine(line: String): Option[Command] =
      parseAll(phrase(command), line) match {
        case Success(out, _) => Some(out)
        case Failure(msg, _) => None
      }

  }

  def help {
    println("""Commands are:
        |help      Show this message
        |graph [agents|ports]  Dump a gexf format file of all the agent or
        |          port connections
        |show <agentName>   Show the connections into and out of an agent.
        |          The agent name includes angle brackets, e.g. <drummer1>
        |snapshot  Capture the state of all the agents' connections"""
        .stripMargin)
  }
  def snapshot: Set[Connection] = unifiedConnections

  /**
   * Output the gexf file.
   * @param gtype  Whether the graph should be ports (with their agents) or just agents
   * @param conns  The port connections (the state)
   */
  def writeGraph(command: GraphCommand)(conns: Conns) {
    import Graphable._

    val filename = "C:\\cygwin\\home\\Nik\\graph\\output.gexf"
    val out = new FileWriter(filename)
    out write Graphable.gexfHeader

    val localConns = conns.agentPortConnections
    val agentConns = conns.agentAgentConnections

    out write "<nodes>\n"

    // Declare the agent nodes
    localConns foreach { out write _._1.nodeXML + "\n" }

    // Maybe declare the port nodes
    command match {
      case GraphPorts => conns.ports foreach { out write _.nodeXML + "\n" }
      case GraphAgents => ; // Do nothing
    }

    out write "</nodes>\n"

    out write "<edges>\n"

    command match {
      // When graphing ports: Write port-port edges and agent-port edges
      case GraphPorts => {
        conns foreach { out write _.edgeXML + "\n" }
        localConns foreach { out write _.edgeXML + "\n" }
      }
      // When graphing agents: Write agent-agent-edges
      case GraphAgents => {
        agentConns foreach { out write _.edgeXML + "\n" }
      }
    }
    out write "</edges>\n"

    out write Graphable.gexfFooter
    out.close

    println("Output to " + filename)
  }

  /**
   * Take all the (port) connections and make sure all the unnamed ports
   * are given names, if such names are known elsewhere.
   */
  def unifiedConnections: Set[Connection] = {
    import Graphable._

    val bls = new BLs("<main>")
    val agents = bls.agents
    val allConnections = for {
      agent <- agents
      bcat = new BCat(agent)
      conn <- bcat.connections
    } yield { println("Agent " + agent + ", connection " + conn); conn }
    val unifiedConnections = allConnections.normalised.unified
    unifiedConnections foreach { c => println("Unified: " + c) }
    unifiedConnections
  }

  def show(agent: String, state: Conns): Unit = {
    val links: Set[(String, String, String)] = for {
      conn <- state
      val masterAgent = conn.master.agent
      val slaveAgent = conn.slave.agent
      if (masterAgent == Some(agent) || slaveAgent == Some(agent))
      val masterName = conn.master.nonEmptyName
      val slaveName = conn.slave.nonEmptyName
      val link = if (masterAgent == Some(agent)) ("", masterName, slaveName)
      else (masterName, slaveName, "")
    } yield link
    
    if (links.size == 0)
      println("No agent called " + agent)
    else {
      val padder = new Padder(links.toSeq, " --> ")
      padder.output foreach { println(_) }
    }
  }
}
