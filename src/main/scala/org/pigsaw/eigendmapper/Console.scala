package org.pigsaw.eigendmapper

import java.io.FileWriter
import scala.util.parsing.combinator.RegexParsers

object Console {

  def main(args: Array[String]) {
    val parser = new ConsoleParser
    val ul = new UserLine(">> ")
    actLoop(new Setup(Set()))

    def actLoop(setup: Setup): Unit = {
      ul.line match {
        case None => ; // Do nothing and hence exit
        case Some(line) => {
          val state2 = act(line, setup)
          actLoop(state2)
        }
      }
    }

    def act(line: String, setup: Setup): Setup = {
      parser.parseLine(line) match {
        /*case Some(Snapshot) => snapshot
        case Some(GraphPorts) => writeGraph(GraphPorts)(setup); setup
        case Some(GraphAgents) => writeGraph(GraphAgents)(setup); setup
        case Some(Show(agent)) => show(agent, setup); setup
        */
        case Some(command) => command(setup)
        case None => println("Unknown command"); setup
      }
    }
  }

  /**
   * Output the gexf file.
   * @param gtype  Whether the graph should be ports (with their agents) or just agents
   * @param conns  The port connections (the state)
   */
  /*def writeGraph(command: GraphCommand)(state: Setup) {
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
    command match {
      case GraphPorts => state.ports foreach { out write _.nodeXML + "\n" }
      case GraphAgents => ; // Do nothing
    }

    out write "</nodes>\n"

    out write "<edges>\n"

    command match {
      // When graphing ports: Write port-port edges and agent-port edges
      case GraphPorts => {
        state.conns foreach { out write _.edgeXML + "\n" }
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
  */

  /*object Snapshot extends Command
abstract class GraphCommand extends Command
object GraphPorts extends GraphCommand
object GraphAgents extends GraphCommand
case class Show(agent: String) extends Command
object Help extends Command
*/
}

class ConsoleParser extends RegexParsers {
  override type Elem = Char

  val commands = List(
      SnapshotCommand,
      ShowCommand,
      HelpCommand)

  // A parser for a single command. It outputs a parser which has already
  // been fed the additional arguments, and just needs a setup to process.
  def oneCommandParser(cmd: Command): Parser[(Setup) => Setup] =
    cmd.command ~> (word *) ^^ { case words => ((s: Setup) => cmd.action(words)(s)) }

  // A word following the main command. There may be several of these
  // making up the command's arguments.
  def word = """\S+""".r

  // A parser for any possible command
  def command = commandsParser(commands)

  // A parser of any number of commands, but at least one. The final parser (if all known
  // commands fail) is one which reports an unknown command
  def commandsParser(cmds: List[Command]): Parser[(Setup) => Setup] = cmds match {
    case Nil         => throw new Exception("Must have at least one command")
    case cmd :: Nil  => oneCommandParser(cmd)
    case cmd :: tail => oneCommandParser(cmd) | commandsParser(tail)
  }

  /*def command = snapshot | graphPorts | graphAgents | show | help

  def snapshot = "snapshot" ^^ { _ => Snapshot }
  def graphPorts = "graph" ~ "ports" ^^ { _ => GraphPorts }
  def graphAgents = "graph" ~ "agents" ^^ { _ => GraphAgents }
  def show = "show" ~> agent ^^ { Show(_) }
  def agent = """\S+""".r
  */

  def parseLine(line: String): Option[(Setup) => Setup] =
    parseAll(phrase(command), line) match {
      case Success(out, _) => Some(out)
      case Failure(msg, _) => None
    }

}
