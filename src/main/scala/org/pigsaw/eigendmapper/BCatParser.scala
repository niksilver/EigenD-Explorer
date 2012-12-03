package org.pigsaw.eigendmapper

import scala.util.parsing.combinator.RegexParsers

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