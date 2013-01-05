package org.pigsaw.eigendmapper

import java.util.regex.Pattern

object Preamble {

  /**
   * Calculate a value, then do something with it before returning it.
   */
  class ReturnableAfter[A](a: A) {
    def returnedAfter(fn: A => Unit): A = { fn(a); a }
  }
  implicit def Any2ReturnableAfter[A](a: A) = new ReturnableAfter(a)

  /**
   * Calculate and return a value after printing a message
   */
  class ButFirstPrintable[A](a: =>A) {
    def butFirstPrint(s: String): A = { println(s); a }
  }
  implicit def Any2ButFirstPrintable[A](a: =>A) = new ButFirstPrintable(a)

  /**
   * A wrapper for methods appropriate to both agents and port IDs.
   */
  case class AgentOrPortID(str: String) {
    import AgentOrPortID._

    lazy private val AgentOrPortIDRE(agent, qualOrNull, baseName, nodePart) = str

    /**
     * The qualifier string in this.
     * E.g. in `"<main.rig1:ahdsr1>"` it will be the string `"main.rig1:"`
     * Returns the empty string if there is no qualifier.
     */
    lazy val qualifier: String =
      if (qualOrNull == null) "" else qualOrNull

    /**
     * Force the fully qualified name of this agent or port to be the given position.
     * {{{
     * "<ag1>" + List()                   => <main:ag1>
     * "<ag1>" + List("<rig1>")           => <main.rig1:ag1>
     * "<ag1>" + List("<rig1>", "<rig2>") => <main.rig1:main.rig2:ag1>
     * }}}
     */
    def qualified(pos: List[String]): String =
      "<" + pos.qualifier + baseName + ">" + nodePart

    /**
     * If this agent or port ID has no explicit qualifier then default it
     * to the one given.
     */
    def defaultQualifier(pos: List[String]): String =
      if (qualifier == "")
        "<" + pos.qualifier + baseName + ">" + nodePart
      else
        str

    /**
     * Get the agent or port ID with an unqualified version of the agent name, which means
     * without all the rig position information.
     */
    def unqualified: String =
      if (qualifier == "") str
      else "<" + baseName + ">" + nodePart

    /**
     * Get the agent or port ID with an unqualified version of the agent name,
     * but only if it is at the given position.
     */
    def unqualifiedForPos(p: List[String]): String =
      if (hasPos(p)) unqualified
      else str

    /**
     * If this agent or port ID is at the given pos.
     * An unqualified agent will have pos `List()`.
     */
    def hasPos(p: List[String]): Boolean =
      (qualifier == "" && p.isEmpty) ||
        (qualifier == p.qualifier)

    /**
     * The pos of this agent or port ID
     */
    def pos: List[String] =
      qualifier match {
        case "" => List()
        case "main:" => List()
        case q => q split ":" map { "<" + _.drop(5) + ">" } toList
      }
  }

  object AgentOrPortID {
    // Groups are:
    //   full agent name with angle brackets,
    //   qualifier with colon (or null),
    //   base name
    //   node part including separator (or empty), e.g. "#34.2" or " beat input" 
    private val AgentOrPortIDRE = """(<([^>]*:)?([^>]*)>)(.*)""".r
  }

  implicit def String2AgentOrPortID(s: String): AgentOrPortID = new AgentOrPortID(s)

  /**
   * The name of an agent, including the angle brackets.
   */
  case class AgentName(name: String) {
    /**
     * Get the name without the angle brackets (if any).
     */
    def withoutBrackets: String = {
      val strip1 = name.dropWhile(_ == '<')
      if (strip1.endsWith(">")) strip1.init else strip1
    }
    
    /**
     * True if the name is a well-formed agent name,
     * including the angle brackets.
     */
    def isAgent = Pattern.matches("<([^>]*:)?([^>]*)>", name)

    /**
     * True if this agent is a rig
     */
    def isRig: Boolean = Pattern.matches("<rig\\d+>", name.unqualified)
  }

  implicit def String2AgentName(s: String): AgentName = new AgentName(s)

  /**
   * A port ID, which consists of the agent name and either the
   * node ID or the cname. Formats will be:
   * `<name1>#12.34.45` or `<name1> beat bar input`.
   * @throws IllegalArgumentException  If the agent and/or label cannot be extracted.
   */
  case class PortID(id: String) {
    import PortID._

    private val PortIDRE(agent0, sep0, label0) = id

    /**
     * Get the agent name, including the angle brackets.
     */
    val agent: String = agent0

    /**
     * Get the node label (the node ID or the node CName).
     * E.g. in `"<cycler1>#4.56"` it is `"4.56"'
     * and in `"<cycler1> beat input"` it is `"beat input"'
     */
    def nodeLabel: String = label0

    /**
     * Get the node label (the node ID or the node CName), and include
     * the `#` if it's a node ID.
     * E.g. in `"<cycler1>#4.56"` it is `"#4.56"'
     * and in `"<cycler1> beat input"` it is `"beat input"'
     */
    def nodeLabelWithHash: String =
      (if (sep0 == "#") "#" else "") + label0
  }

  object PortID {
    private val PortIDRE = """(<[^>]*>)([# ])(.+)""".r
  }

  implicit def string2PortID(id: String) = new PortID(id)

  /**
   * A position in a rig hierarchy in which we're currently
   * interested. An empty list means the top level; `List("<&lt;rig3>")`
   * means rig3 within the top level, and so on.
   */
  case class Pos(p: String*) {
    /**
     * Convert a pos to an index specification for the bls command:
     * {{{
     * List()                   => <main>
     * List("<rig1>")           => <main.rig1:main>
     * List("<rig1>", "<rig2>") => <main.rig1:main.rig2:main>
     * }}}
     */
    def index: String = {
      def insertMains(s: String) = "." + s.withoutBrackets + ":main"
      "<main" + (p map insertMains).mkString + ">"
    }

    /**
     * The qualifier needed in an agent for this pos.
     * E.g. `List("<rig1>")` yields `"main.rig1:"`
     * and `List()` yields `"main:"`
     */
    def qualifier: String =
      if (p.isEmpty)
        "main:"
      else
        (p map { "main." + AgentName(_).withoutBrackets + ":" }) mkString

    def displayString: String =
      if (p.isEmpty) "Top level"
      else p.mkString(" - ")

  }

  implicit def ListString2Pos(p: List[String]): Pos = Pos(p: _*)

  /**
   * An ordering on elements of a string, or ints.
   * Ints are "less than" chars, otherwise they are ordered naturally.
   */
  def lessThanStringElt(a: Either[Int, Char], b: Either[Int, Char]): Boolean = {
    (a, b) match {
      case (Left(i1), Left(i2)) => i1 < i2
      case (Right(c1), Right(c2)) => c1 < c2
      case (Left(_), Right(_)) => true
      case (Right(_), Left(_)) => false
    }
  }

  /**
   * An ordering on strings, in which int sequences are considered
   * numerically, and any int sequence is less than a char.
   * Thus `"agent1" < "agent2" < "agent10" < "agentA"`.
   */
  def lessThanAlphaInts(a: String, b: String): Boolean = {
    
    val IntTail = """(\d{1,9})(.*)""".r
    def split(s: String): (Either[Int,Char], String) =
      s match {
        case IntTail(s1, st) => (Left(s1.toInt), st)
        case _ => (Right(s.head), s.tail)
      }
    
    if (a == "")
      (b != "")
    else if (b == "")
      false
    else {
      val (aHead, aTail) = split(a)
      val (bHead, bTail) = split(b)

      if (aHead == bHead)
        lessThanAlphaInts(aTail, bTail)
      else
        lessThanStringElt(aHead, bHead)
    }
  }
}