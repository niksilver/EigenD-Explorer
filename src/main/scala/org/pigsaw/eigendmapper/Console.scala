package org.pigsaw.eigendmapper

object Console {
  def main(args: Array[String]) {
    val ul = new UserLine(">> ")
    val line = ul.line
    println("Output is : " + line + ", length is " + (line getOrElse("")).length)
  }
  
  def x_main(args: Array[String]) {
    import Graphable._
    
    val filename = "C:\\cygwin\\home\\Nik\\graph\\output.gexf"
    val out = new java.io.FileWriter(filename)
    out write Graphable.gexfHeader
    
    val conns = unifiedConnections
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