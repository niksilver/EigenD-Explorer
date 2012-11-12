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
  /** Optional inline whitepace only. */
  def whitespace0 = """[ \t]*""".r
  
  /** Create a parser for a string with whitespace around it. */
  def ws(s: String) = whitespace0 ~> s <~ whitespace0
  def stateVariableName = """\d+(\.\d+)*""".r
  def stateVariableString = """.*""".r
  
  def outputLine = stateVariableName ~ whitespace ~ stateVariableString ^^
    { case name ~ ws ~ string => StateVariableLine(name, string) }
  
  def dictionary = ws("{") ~> (keyValuePairs ?) <~ ws("}") ^^ {
    case Some(map) => map
    case None => Map()
  }
  
  def keyValuePairs = keyValuePair ~ (( ws(",") ~> keyValuePair )*) ^^ {
    case (key, value) ~ List() => Map(key -> value)
    case (key, value) ~ keyValues => Map(key -> value) ++ keyValues.toMap
  } 
  
  def keyValuePair = key ~ ws(":") ~ value ^^ { case key ~ colon ~ value => (key, value) }
  
  def key = """\w+""".r
  def value: Parser[String] = (( bracketValue | unprotectedValue ) +) ^^ { _.mkString }
  
  /** A value that doesn't have surrounding protection characters such as (...) or '...' */
  def unprotectedValue = """[^\]\[()'<>{} ,:]+""".r
  
  /** A value that does have surrounding protection characters such as (...) or '...' */
  def protectedValue = """[^\]\[()'<>{}]+""".r

  /** A value with brackets of some kind. */
  def bracketValue = parentheticalValue
  
  /* A value that's inside brackets of some kind. */
  def bracketedValue: Parser[String] = (( bracketValue | protectedValue ) +) ^^ { _.mkString }
  
  /** A value that does have surrounding protection characters such as (...) or '...' */
  def parentheticalValue = "(" ~ bracketedValue ~ ")" ^^ { case op ~ value ~ cl => op + value + cl }
  
  def parseWhole[T](parser: Parser[T], dictstr: String): Option[T] =
    parseAll(parser, dictstr) match {
    case Success(out, _) => Some(out)
    case fail => None
  }
}

case class StateVariableLine(name: String, string: String)
