package org.pigsaw.eigendmapper

import Preamble._

import scala.sys.process.Process

object EigenD {
  val bin = "C:\\Program Files (x86)\\Eigenlabs\\release-2.0.68-stable\\bin"

  /**
   * The output of an EigenD command, with newlines omitted.
   * @param command  The command, e.g. <code>"bls.exe &lt;main&gt;"</code>
   */
  def exec(command: String): Stream[String] = Process(EigenD.bin + "/" + command).lines
}
/**
 * A bls command.
 * @param index  The index being listed, including angle brackets.
 *               E.g. &lt;main&gt;
 */
class BLs(index: String) {
  /**
   * The text output of the <code>bls <i>index</i></code> command, line by line.
   */
  def text: Stream[String] = EigenD.exec("bls.exe " + index)

  /**
   * Get the agents from a stream, which is expected to be the output
   * of the bls.exe command. Agents come out of that command as:
   * <pre>
   *   &lt;agent 1 name&gt;
   *   &lt;agent 2 name&gt;
   *   etc
   * </pre>
   * and this will return a list of the agent names, including the angle brackets.
   */
  def agents: List[String] =
    (text flatMap ("(<.*>)".r unapplySeq (_)) flatten).toList

}

/**
 * A bcat command with the agent name.
 * @param agent  The agent name, including angle brackets and ordinal.
 */
class BCat(val agent: String) {

  /**
   * The text output of the bcat command, line by line.
   */
  def text: Stream[String] = EigenD.exec("bcat.exe " + agent)

  /**
   * A translation of the bcat text into a map of state nodes IDs and values.
   */
  def state: Map[String, StateValue] = {
    val parser = new BCatParser
    val lineOptions = for (line <- text) yield parser.parseLine(line)
    lineOptions.flatten.toMap
  }

  /**
   * The set of all master/slave connections that involve this agent
   * on one side or the other.
   */
  lazy val connections: Set[Connection] = {
    val conns = for {
      stateNodeIDValue <- state
      stateNodeID = stateNodeIDValue._1
      dictValue <- stateNodeIDValue._2.dictValue.seq
      dict = dictValue.dict
      portCName = dict.get("cname") map { _.mkString }
      (key, valueList) <- dict
      conn <- key match {
        case "slave" => slaveConnections(portCName, stateNodeID, valueList)
        case "master" => masterConnections(portCName, stateNodeID, valueList)
        case _ => List()
      }
    } yield conn
    conns.toSet
  }

  def slaveConnections(portCName: Option[String], stateNodeID: String, valueList: List[String]): List[Connection] = {
    for {
      slave <- valueList
      slaveStripped = slave.stripPrefix("'").stripSuffix("'")
      masterPort = Port(agent + "#" + stateNodeID, portCName)
      slavePort = Port(slaveStripped, None)
    } yield Connection(masterPort, slavePort)
  }

  def masterConnections(portCName: Option[String], stateNodeID: String, valueList: List[String]): List[Connection] = {
    for {
      masterConn <- valueList
      master = masterConn.split(',')(2).stripPrefix("'").stripSuffix("'")
      masterPort = Port(master, None)
      slavePort = Port(agent + "#" + stateNodeID, portCName)
    } yield Connection(masterPort, slavePort)
  }
  
  /**
   * Get the settings in this agent. Each key value pair is the port ID
   * and its value.
   */
  def settings: Map[String, String] = {
    for {
      stateNodeIDValue <- state
      stateNodeID = stateNodeIDValue._1
      if stateNodeID.endsWith(".254")
      // Now turn 1.2.3.254 into 1.2.3
      setNode = stateNodeID.dropRight(4)
      strValue <- stateNodeIDValue._2.stringValue.seq
      nodeID = agent.unqualified + "#" + setNode
    } yield (nodeID -> strValue)
  }
}
