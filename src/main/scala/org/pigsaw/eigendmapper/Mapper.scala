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
  
  def stateVariableName = """\d+(\.\d+)*""".r
  def stateVariableString = """.*""".r
  
  def outputLine = stateVariableName ~ stateVariableString ^^
    { case name ~ string => StateVariableLine(name, string) }
  
  def dictionary = "{" ~> (keyValuePairs ?) <~ "}" ^^ {
    case Some(map) => map
    case None => Map()
  }
  
  def keyValuePairs = keyValuePair ~ (("," ~> keyValuePair)*) ^^ {
    case (key, value) ~ List() => Map(key -> value)
    case (key, value) ~ keyValues => Map(key -> value) ++ keyValues.toMap
  } 
  
  def keyValuePair = key ~ ":" ~ value ^^ { case key ~ colon ~ value => (key, value) }
  
  def key = """\w+""".r
  def value: Parser[String] = (( parentheticalValue | unifiedValue ) +) ^^ { _.mkString }
  
  // A value that doesn't have surrounding protection characters such as (...) or '...'
  def unifiedValue = """[^\]\[ ()'<>{},:]+""".r
  def parentheticalValue = "(" ~ value ~ ")" ^^ { case op ~ value ~ cl => op + value + cl }
  
  def parseWhole[T](parser: Parser[T], dictstr: String): Option[T] =
    parseAll(parser, dictstr) match {
    case Success(out, _) => Some(out)
    case fail => None
  }
}

case class StateVariableLine(name: String, string: String)
