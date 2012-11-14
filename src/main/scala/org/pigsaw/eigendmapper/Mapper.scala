package org.pigsaw.eigendmapper

import scala.util.parsing.combinator.RegexParsers

object Mapper {
  
  /** Get the agents from a stream, which is expected to be the output
   * of the bls.exe command. Agents come out of that command as:
   * <pre>
   *   &lt;agent 1 name&gt;
   *   &lt;agent 2 name&gt;
   *   etc
   * </pre>
   * and this will return a list of the agent names, without the angle brackets.
   */
  def filterAgents(in: Stream[String]): List[String] =
    (in flatMap ("<(.*)>".r unapplySeq(_)) flatten).toList
  
}

class BCatOutputParser extends RegexParsers {
  override type Elem = Char
  override def skipWhitespace = false
  
  /** Positive inline whitepace only. */
  def whitespace = """[ \t]+""".r
  
  def stateVariableName = """\d+(\.\d+)*""".r
  def stateVariableString = """.*""".r
  
  def outputLine = stateVariableName ~ whitespace ~ stateVariableString ^^
    { case name ~ ws ~ string => StateVariableLine(name, string) }
  
  def dictionary = "{" ~> keyValuePairs <~ "}"

  /** Key/value pairs have to be parsed like this because the comma is used
   * to separate key/value pairs and items in a value list, and the default
   * parsers don't do backtracking.
   */
  def keyValuePairs = ((value | key | ":" | ",")*) ^^ { keyValueStringsToMap(_) }
  
  def key = """\w+""".r
  def value = (( bracketValue | quoteValue | bareValue ) +) ^^ { _.mkString }
  
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
  def nonEmptyBracketedValue = (( bracketValue | quoteValue | bracketedBareValue ) +) ^^ { _.mkString }
  
  def parentheticalValue = "(" ~ bracketedValue ~ ")" ^^ { case op ~ value ~ cl => op + value + cl }
  def angleBracketValue  = "<" ~ bracketedValue ~ ">" ^^ { case op ~ value ~ cl => op + value + cl }
  def bracesValue        = "{" ~ bracketedValue ~ "}" ^^ { case op ~ value ~ cl => op + value + cl }
  def squareBracketValue = "[" ~ bracketedValue ~ "]" ^^ { case op ~ value ~ cl => op + value + cl }
  
  type Values = List[String]
  type Dictionary = Map[String, Values]
  
  def keyValueStringsToMap(kvs: List[String]): Dictionary = kvs match {
    case Nil => Map()
    case key :: ":" :: remainder => stringsToMapLoadKey(key, List(), Map(), remainder)
    case x => stringsToMapLoadKey("Unparsed", x, Map(), List())
  }
  
  def stringsToMapLoadKey(key: String, vals: Values, accum: Dictionary, remainder: List[String]): Dictionary =
    remainder match {
    case Nil => accum + (key -> vals)
    case "," :: tail => stringsToMapLoadKey(key, vals, accum, tail)
    case str :: ":" :: tail => stringsToMapLoadKey(str, List(), accum + (key -> vals), tail)
    case str :: tail => stringsToMapLoadKey(key, vals :+ str, accum, tail)
  }
  
}

case class StateVariableLine(name: String, string: String)
