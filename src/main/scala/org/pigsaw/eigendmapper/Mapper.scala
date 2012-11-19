package org.pigsaw.eigendmapper

import scala.util.parsing.combinator.RegexParsers
import scala.sys.process.Process
import scala.collection.immutable.HashSet

object Mapper {
  def main(args: Array[String]) {
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
  }
}

object EigenD {
  val bin = "C:\\Program Files (x86)\\Eigenlabs\\release-2.0.68-stable\\bin"

  /**
   * The output of an EigenD command, with newlines omitted.
   * @param command  The command, e.g. <code>"bls.exe &lt;main&gt;"</code>
   */
  def exec(command: String): Stream[String] = Process(EigenD.bin + "/" + command).lines
}

/**
 * Graph utils.
 */
class Graphable(val conns: Set[Connection]) {
  /**
   * Create a unified set of connections. This means if any connections
   * carry a port name, then those names are applied wherever those
   * ports are used.
   */
  def unified: Set[Connection] = {
    val ports = conns flatMap { c => List(c.master, c.slave) }
    val namingPorts = ports filter ( _.name.nonEmpty )
    val names: Map[String, String] = namingPorts map { p => (p.id -> p.name.get) } toMap

    // Produce an updated version of the port, with names filled in if available.
    def updated(port: Port): Port = {
      if (port.name.nonEmpty) port
      else Port(port.id, names.get(port.id))
    }

    conns map (c => Connection(updated(c.master), updated(c.slave)))
  }

  /**
   * Make a normalised version of this set of connections, in which
   * every port of the form ID &lt;main:agentnameN&gt; is changed to
   * its shorter form of &lt;agentnameN&gt;.
   */
  def normalised: Set[Connection] =
    conns map { _.normalised }

  /**
   * Get all the agent names mentioned in the set of connections,
   * including the angle brackets.
   */
  def agents: Set[String] =
    conns flatMap { _.agents }
}

object Graphable {
  implicit def setConnection2Graphable(conns: Set[Connection]): Graphable = new Graphable(conns)
  implicit def listConnection2Graphable(conns: List[Connection]): Graphable = new Graphable(conns.toSet)
  implicit def port2GraphableString(s: String) = new GString(s)
  implicit def port2GraphablePort(p: Port) = new GPort(p)
  implicit def port2GraphableConnection(c: Connection) = new GConnection(c)
  
  val gexfHeader =
    """<?xml version="1.0" encoding="UTF-8"?>
      |    <gexf xmlns="http://www.gexf.net/1.2draft" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.gexf.net/1.2draft http://www.gexf.net/1.2draft/gexf.xsd" version="1.2">
      |        <graph mode="static" defaultedgetype="directed">""".stripMargin
  val gexfFooter =
    """    </graph>
      |</gexf>""".stripMargin
  
  class GString(s: String) {
    lazy val xmlId: String = "[^A-Za-z0-9.]".r replaceAllIn (s, "_")
  }
  
  class GPort(p: Port) {
    lazy val xmlId: String = p.id.xmlId
    
    lazy val nodeXML: String = {
      val label = p.name getOrElse p.id
      """<node id="%s" label="%s" />""".format(p.xmlId, label)
    }
  }
  
  class GConnection(c: Connection) {
    lazy val xmlId: String = c.master.xmlId + c.slave.xmlId
    
    lazy val edgeXML: String = {
      val template = """<edge id="%s" source="%s" target="%s" weight="5" />"""
      template.format(c.xmlId, c.master.xmlId, c.slave.xmlId)
    }
  }
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
class BCat(agent: String) {

  /**
   * The text output of the bcat command, line by line.
   */
  def text: Stream[String] = EigenD.exec("bcat.exe " + agent)

  /**
   * A translation of the bcat text into a map of state variables and values.
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
      stateVarValue <- state
      stateVar = stateVarValue._1
      dictValue <- stateVarValue._2.dictValue.seq
      dict = dictValue.dict
      portCName = dict.get("cname") map { _.mkString }
      (key, valueList) <- dict
      conn <- key match {
        case "slave" => slaveConnections(portCName, stateVar, valueList)
        case "master" => masterConnections(portCName, stateVar, valueList)
        case _ => List()
      }
    } yield conn
    conns.toSet
  }

  def slaveConnections(portCName: Option[String], stateVar: String, valueList: List[String]): List[Connection] = {
    for {
      slave <- valueList
      slaveStripped = slave.stripPrefix("'").stripSuffix("'")
      masterPort = Port(agent + "#" + stateVar, portCName)
      slavePort = Port(slaveStripped, None)
    } yield Connection(masterPort, slavePort)
  }

  def masterConnections(portCName: Option[String], stateVar: String, valueList: List[String]): List[Connection] = {
    for {
      masterConn <- valueList
      master = masterConn.split(',')(2).stripPrefix("'").stripSuffix("'")
      masterPort = Port(master, None)
      slavePort = Port(agent + "#" + stateVar, portCName)
    } yield Connection(masterPort, slavePort)
  }
}

/**
 * A port in an agent that might be one end of a connection.
 * @param id  Agent name, including angle brackets and ordinal, and port
 *               e.g. "&lt;metronome1&gt#3.6;
 * @param name  The name of the port if known, e.g. "bar beat output"
 */
case class Port(val id: String, val name: Option[String]) {
  /**
   * Generate a normalised version of this port. I.e. If the id is of
   * the form &lt;<main:agentnameN&gt; then it's converted to &lt;agentnameN&gt;.
   */
  def normalised: Port =
    if (id.startsWith("<main:")) Port("<" + id.drop(6), name)
    else this

  /**
   * Get the agent name embedded in the port id, including the angle brackets.
   */
  def agent: Option[String] = "(<.*>)".r findFirstIn(id)

}

case class Connection(val master: Port, val slave: Port) {
  /**
   * Generate a normalised version of this connection. I.e. If the id of either
   * the master or the slave is of the form &lt;<main:agentnameN&gt; then it's
   * converted to &lt;agentnameN&gt;.
   */
  def normalised: Connection = {
    val normMaster = master.normalised
    val normSlave = slave.normalised
    if ((normMaster eq master) && (normSlave eq slave)) this
    else Connection(normMaster, normSlave)
  }
  
  /**
   * Get the agent names embedded in the master and slave port ids.
   */
  def agents: Set[String] = Set() ++ master.agent.toSeq ++ slave.agent.toSeq
}

class BCatParser extends RegexParsers {
  override type Elem = Char
  override def skipWhitespace = false

  /** Positive inline whitepace only. */
  def whitespace = """[ \t]+""".r

  def stateVariableName = "." | """\d+(\.\d+)*""".r
  def stringValue = """.*""".r
  def wrappedStringValue = stringValue ^^ { StringValue(_) }

  def stateValue = wrappedDictionary | wrappedStringValue

  def outputLine = stateVariableName ~ whitespace ~ stateValue ^^
    { case name ~ ws ~ state_value => (name -> state_value) }

  def dictionary = "{" ~> keyValuePairs <~ "}"
  def wrappedDictionary = dictionary ^^ { DictValue(_) }

  /**
   * Key/value pairs have to be parsed like this because the comma is used
   * to separate key/value pairs and items in a value list, and the default
   * parsers don't do backtracking.
   */
  def keyValuePairs = ((value | key | ":" | ",")*) ^^ { keyValueStringsToMap(_) }

  def key = """\w+""".r
  def value = ((bracketValue | quoteValue | bareValue) +) ^^ { _.mkString }

  /** A value that doesn't have surrounding protection characters such as (...) or '...' */
  def bareValue = """[^\]\[()'<>{},:]+""".r

  /** A value that does have surrounding protection characters such as (...) or '...' */
  def bracketedBareValue = """[^\]\[()'<>{}]+""".r

  /** A value surrounded by quotes. */
  def quoteValue = """'[^']*'""".r

  /** A value with brackets of some kind. */
  def bracketValue = parentheticalValue | angleBracketValue | bracesValue | squareBracketValue

  /** A value that's inside brackets of some kind. */
  def bracketedValue: Parser[String] = nonEmptyBracketedValue | ""
  def nonEmptyBracketedValue = ((bracketValue | quoteValue | bracketedBareValue) +) ^^ { _.mkString }

  def parentheticalValue = "(" ~ bracketedValue ~ ")" ^^ { case op ~ value ~ cl => op + value + cl }
  def angleBracketValue = "<" ~ bracketedValue ~ ">" ^^ { case op ~ value ~ cl => op + value + cl }
  def bracesValue = "{" ~ bracketedValue ~ "}" ^^ { case op ~ value ~ cl => op + value + cl }
  def squareBracketValue = "[" ~ bracketedValue ~ "]" ^^ { case op ~ value ~ cl => op + value + cl }

  type Values = List[String]
  type Dictionary = Map[String, Values]

  /**
   * Take a series of strings that make up the key value sequence and turn it into
   * a mapping of keys to (lists of) values. Strings will include the separating
   * colons and commas. E.g.:
   * <pre>
   *     key1 : value1 , key2 : value2a , value2b
   * </pre>
   */
  def keyValueStringsToMap(kvs: List[String]): Dictionary = kvs match {
    case Nil => Map()
    case key :: ":" :: remainder => stringsToMapLoadKey(key, List(), Map(), remainder)
    case x => stringsToMapLoadKey("Unparsed", x, Map(), List())
  }

  /**
   * Helper function for <code>keyValueStringsToMap</code>.
   * @param key  The key whose values are currently being collected
   * @param vals  The values collected so far for the given key
   * @param accum  The accumulated mapping of keys to values, excluding the key
   *               that's currently being worked on
   * @param remainder  The key value string (and separator) list that's yet to be processed.
   */
  def stringsToMapLoadKey(key: String, vals: Values, accum: Dictionary, remainder: List[String]): Dictionary =
    remainder match {
      case Nil => accum + (key -> vals)
      case "," :: tail => stringsToMapLoadKey(key, vals, accum, tail)
      case str :: ":" :: tail => stringsToMapLoadKey(str, List(), accum + (key -> vals), tail)
      case str :: tail => stringsToMapLoadKey(key, vals :+ str, accum, tail)
    }

  def parseLine(line: String): Option[(String, StateValue)] =
    parseAll(phrase(outputLine), line) match {
      case Success(out, _) => Some(out)
      case Failure(msg, _) => None
    }

}

sealed abstract class StateValue {
  def stringValue: Option[String]
  def dictValue: Option[DictValue]
}

case class StringValue(value: String) extends StateValue {
  def stringValue = Some(value)
  def dictValue = None
}

case class DictValue(dict: Map[String, List[String]]) extends StateValue {
  def stringValue = None
  def dictValue = Some(this)
}

