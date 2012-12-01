package org.pigsaw.eigendmapper

object Console {
  def main(args: Array[String]) {
    val ul = new UserLine(">> ")
    actLoop(Set())

    def actLoop(state: Set[Connection]): Unit = {
      ul.line match {
        case None => ; // Do nothing
        case Some(input) => {
          val state2 = act(input, state)
          actLoop(state2)
        }
      }
    }

    def act(line: String, state: Set[Connection]): Set[Connection] = {
      line match {
        case "snapshot" => snapshot
        case "graph agents" => graphAgents(state); state
        case _ => println("Unknown command"); state
      }
    }
  }
  
  def snapshot: Set[Connection] = unifiedConnections

  def graphAgents(conns: Set[Connection]) {
    import Graphable._

    val filename = "C:\\cygwin\\home\\Nik\\graph\\output.gexf"
    val out = new java.io.FileWriter(filename)
    out write Graphable.gexfHeader

    val localConns = conns.agentPortConnections
    val agentConns = conns.agentAgentConnections

    out write "<nodes>\n"
    conns.ports foreach { out write _.nodeXML + "\n" }
    localConns foreach { out write _._1.nodeXML + "\n" }
    out write "</nodes>\n"

    out write "<edges>\n"
    conns foreach { out write _.edgeXML + "\n" }
    localConns foreach { out write _.edgeXML + "\n" }
    //agentConns foreach { out write _.edgeXML + "\n" }
    out write "</edges>\n"

    out write Graphable.gexfFooter
    out.close
    
    println("Output to " + filename)
  }

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