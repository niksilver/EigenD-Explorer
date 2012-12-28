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
  type Dict = Map[String, List[String]]

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
   * Get a mapping from all node IDs to their dictionaries -- but only for those
   * nodes which have a dictionary.
   */
  private lazy val nodeIDDicts: Map[String, Dict] =
    for {
      stateNodeIDValue <- state
      stateNodeID = stateNodeIDValue._1
      dict <- stateNodeIDValue._2.dictValue.seq
    } yield (stateNodeID -> dict)

  /**
   * The set of all master/slave connections that involve this agent
   * on one side or the other.
   */
  lazy val connections: Set[Connection] = {
    val conns = for {
      (stateNodeID, dict) <- nodeIDDicts
      (key, valueList) <- dict
      conn <- key match {
        case "slave" => slaveConnections(stateNodeID, valueList)
        case "master" => masterConnections(stateNodeID, valueList)
        case _ => List()
      }
    } yield conn
    conns.toSet
  }

  def slaveConnections(stateNodeID: String, valueList: List[String]): List[Connection] = {
    for {
      slave <- valueList
      slaveStripped = slave.stripPrefix("'").stripSuffix("'")
      masterPort = agent + "#" + stateNodeID
      slavePort = slaveStripped
    } yield Connection(masterPort, slavePort)
  }

  def masterConnections(stateNodeID: String, valueList: List[String]): List[Connection] = {
    for {
      masterConn <- valueList
      master = masterConn.split(',')(2).stripPrefix("'").stripSuffix("'")
      masterPort = master
      slavePort = agent + "#" + stateNodeID
    } yield Connection(masterPort, slavePort)
  }

  /**
   * Get the (optional) name or cname (and ordinal) from a state
   * dictionary value. Name trumps cname. Name will come with the
   * ordinal, or else the cordinal. Cname will come with the cordinal.
   */
  private def name(dict: Dict): Option[String] = {
    val cordinalSuffix = dict.get("cordinal") match {
      case Some(cord) => " " + cord.mkString.trim
      case None => ""
    }
    val ordinalSuffix = dict.get("ordinal") match {
      case Some(ord) => " " + ord.mkString.trim
      case None => cordinalSuffix
    }
    val optCNameCOrdinal = dict.get("cname") match {
      case Some(cn) => Some(cn.mkString.trim + cordinalSuffix)
      case None => None
    }

    dict.get("name") match {
      case Some(n) => Some(n.mkString.trim + ordinalSuffix)
      case None => optCNameCOrdinal
    }
  }

  /**
   * Get a map from node IDs to their names. Not all node IDs will have
   * a name, of course.
   */
  lazy val nodeIDNames: Map[String, String] = {
    for {
      (stateNodeID, dict) <- nodeIDDicts
      if name(dict).nonEmpty
      val name = simplestUniqueName(stateNodeID)
    } yield (stateNodeID -> name)
  }

  /**
   * Get a map from node IDs to their names. Not all node IDs will have
   * a name, of course.
   */
  lazy val unqualifiedNodeIDNames: Map[String, String] =
    for {
      (stateNodeID, dict) <- nodeIDDicts
      portName <- name(dict).seq
    } yield (stateNodeID -> portName)

  /**
   * Get the simplest unique name for a node ID.
   * If a node ID has a non-unique name then prefix it with the name
   * next up the node tree, and do so repeatedly until the name
   * is unique.
   */
  def simplestUniqueName(nodeID: String): String =
    simplestUniqueName(nodeID, 0)

  private def simplestUniqueName(nodeID: String, steps: Int): String = {
    println("simplestUniqueName(" + nodeID + ", " + steps + ")")
    val candidateName = qualifiedNodeIDName(nodeID, steps)
    var other: String = "undef so far"
    val nonUnique = unqualifiedNodeIDNames exists { nn =>
      other = nn._1
      qualifiedNodeIDName(nn._1, steps) == candidateName && nn._1 != nodeID
    }
    if (nonUnique) {
      println("Found '" + candidateName + "' at " + other)
      simplestUniqueName(nodeID, steps + 1)
    }
    else
      candidateName
  }

  /**
   * Get the node ID name, qualified up the specified number of steps.
   */
  def qualifiedNodeIDName(nodeID: String, steps: Int): String =
    qualifiedNodeIDName(nodeID, steps, "")

  private def qualifiedNodeIDName(nodeID: String, steps: Int, accum: String): String =
    steps match {
      case 0 => accum
      case _ => {
        val name = unqualifiedNodeIDNames.get(nodeID).mkString
        val nodes = nodeID split '.'
        val nextNodeSeq = nodes dropRight 1
        val nextNodeID = nextNodeSeq mkString "."
        val sep = if (accum == "") "" else " "
        qualifiedNodeIDName(nextNodeID, steps - 1, name + sep + accum)
      }
    }

  /**
   * Get the settings in this agent. Each key value pair is the port ID
   * and its value.
   */
  lazy val settings: Map[String, String] = {
    for {
      (stateNodeID, stateValue) <- state
      if stateNodeID.endsWith(".254")
      // Find the node that's been set by going
      // from 1.2.3.254 to 1.2.3
      setNode = stateNodeID.dropRight(4)
      strValue <- stateValue.stringValue.seq
      portID = agent.unqualified + "#" + setNode
      portIDBestForm = portID.bestForm(nodeIDNames)
    } yield (portIDBestForm -> strValue)
  }
}
