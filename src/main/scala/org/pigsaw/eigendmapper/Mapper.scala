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

class DictionaryOutputParser extends RegexParsers {
  override type Elem = Char
  def stateVariableName = """\d+""".r
  def stateVariableString = """.*""".r
  def outputLine = stateVariableName ~ stateVariableString ^^
    { case name ~ string => DictionaryOutput(name, string) }
  
  def parseLine(line: String): Option[DictionaryOutput] =
    parseAll(outputLine, line) match {
    case Success(out, _) => Some(out)
    case _ => None
  }
}

case class DictionaryOutput(name: String, string: String)
