package org.pigsaw.eigendmapper

import java.io.FileWriter

object Console {

  type Conns = Set[Connection]

  def main(args: Array[String]) {
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
      line match {
        case "snapshot" => snapshot
        case "graph ports" =>  writeGraph(PortsWithAgents)(state); state
        case "graph agents" => writeGraph(AgentsToAgents)(state); state
        case _ => println("Unknown command"); state
      }
    }
  }

  def snapshot: Set[Connection] = unifiedConnections

  sealed abstract class GraphType
  object PortsWithAgents extends GraphType
  object AgentsToAgents extends GraphType

  /**
   * Output the gexf file.
   * @param gtype  Whether the graph should be ports (with their agents) or just agents
   * @param conns  The port connections (the state)
   */
  def writeGraph(gtype: GraphType)(conns: Conns) {
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
    gtype match {
      case PortsWithAgents => conns.ports foreach { out write _.nodeXML + "\n" }
      case AgentsToAgents => ; // Do nothing
    }

    out write "</nodes>\n"

    out write "<edges>\n"

    gtype match {
      // When graphing ports: Write port-port edges and agent-port edges
      case PortsWithAgents => {
        conns foreach { out write _.edgeXML + "\n" }
        localConns foreach { out write _.edgeXML + "\n" }
      }
      // When graphing agents: Write agent-agent-edges
      case AgentsToAgents => {
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
}
