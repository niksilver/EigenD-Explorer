package org.pigsaw.eigendmapper

import scala.util.parsing.combinator.RegexParsers
import scala.sys.process.Process

object Mapper {

  /**
   * Get the agents from a stream, which is expected to be the output
   * of the bls.exe command. Agents come out of that command as:
   * <pre>
   *   &lt;agent 1 name&gt;
   *   &lt;agent 2 name&gt;
   *   etc
   * </pre>
   * and this will return a list of the agent names, without the angle brackets.
   */
  def filterAgents(in: Stream[String]): List[String] =
    (in flatMap ("<(.*)>".r unapplySeq (_)) flatten).toList

}

/**
 * A bcat command with the agent name.
 * @param agent  The agent name, including angle brackets
 */
class BCat(agent: String) {

  private val eigend_bin = "C:\\Program Files (x86)\\Eigenlabs\\release-2.0.68-stable\\bin"

  /**
   * The text output of the bcat command, line by line.
   */
  def text: Stream[String] = Process(eigend_bin + "/bcat.exe " + agent).lines

  /**
   * A translation of the bcat text into a map of state variables and values.
   */
  def state: Map[String, StateValue] = {
    val parser = new BCatParser
    val lineOptions = for (line <- text) yield parser.parseLine(line)
    lineOptions.flatten.toMap
  }

  lazy val connections = {
    val conns = for {
      stateVarValue <- state
      stateVar = stateVarValue._1
      dictValue <- stateVarValue._2.dictValue.seq
      dict = dictValue.dict
      agentCName = dict.get("cname") map { _.mkString }
      (key, valueList) <- dict
      conn <- key match {
        case "slave" => slaveConnections(agentCName, stateVar, valueList)
        case _ => List()
      }
    } yield conn
    conns.toSet
  }

  def slaveConnections(agentCName: Option[String], stateVar: String, valueList: List[String]): List[Connection] = {
    for {
      slave <- valueList
      slaveStripped = slave.stripPrefix("'").stripSuffix("'")
      masterPort = Port(agent + "#" + stateVar, agentCName)
      slavePort = Port(slaveStripped, None)
    } yield Connection(masterPort, slavePort)
  }
}

/**
 * A port in an agent that might be one end of a connection.
 * @param id  Agent name, including angle brackets and ordinal, and port
 *               e.g. "&lt;metronome1&gt#3.6;
 * @param name  The name of the port if known, e.g. "bar beat output"
 */
case class Port(id: String, name: Option[String])

case class Connection(master: Port, slave: Port)

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

