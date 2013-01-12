/*
 *  Copyright 2012 Nik Silver.
 *  
 *  This file is part of EigenD Explorer.
 *
 *  EigenD Explorer is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  EigenD Explorer is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with EigenD Explorer.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.pigsaw.eigendexplorer

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
   * Methods appropriate to both agents and port IDs.
   */
  trait AgentOrPortID {
    /**
     * The type of this: either an agent or port ID.
     */
    type T
    
    /**
     * The string representation, such as `<summer1>` or `<gain2>#15.4`.
     */
    val str: String
    
    /**
     * Make one of these from a string
     */
    def make(s: String): T
    
    /**
     * Return this, but of the right type.
     */
    def same: T

    // Groups are:
    //   full agent name with angle brackets,
    //   qualifier with colon (or null),
    //   base name
    //   node part including separator (or empty), e.g. "#34.2" or " beat input" 
    private val AgentOrPortIDRE = """(<([^>]*:)?([^>]*)>)(.*)""".r
    
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
    def qualified(pos: Pos): T =
      make("<" + pos.qualifier + baseName + ">" + nodePart)

    /**
     * If this agent or port ID has no explicit qualifier then default it
     * to the one given.
     */
    def defaultQualifier(pos: Pos): T =
      if (qualifier == "")
        make("<" + pos.qualifier + baseName + ">" + nodePart)
      else
        same

    /**
     * Get the agent or port ID with an unqualified version of the agent name, which means
     * without all the rig position information.
     */
    def unqualified: T =
      if (qualifier == "") same
      else make("<" + baseName + ">" + nodePart)

    /**
     * Get the agent or port ID with an unqualified version of the agent name,
     * but only if it is at the given position.
     */
    def unqualifiedForPos(p: Pos): T =
      if (hasPos(p)) unqualified
      else same

    /**
     * If this agent or port ID is at the given pos.
     * An unqualified agent will have pos `List()`.
     */
    def hasPos(p: Pos): Boolean =
      (qualifier == "" && p.topLevel) ||
        (qualifier == p.qualifier)

    /**
     * The pos of this agent or port ID
     */
    def pos: Pos =
      qualifier match {
        case "" => List()
        case "main:" => List()
        case q => q split ":" map { e => Agent("<" + e.drop(5) + ">") } toList
      }
    
    /**
     * The underlying string, no decoration
     */
    override def toString = str
  }

  //implicit def String2AgentOrPortID(s: String): AgentOrPortID = new AgentOrPortID(s)

  /**
   * The name of an agent, including the angle brackets.
   */
  case class Agent(name: String) extends AgentOrPortID {
    
    val str = name
    type T = Agent
    def make(s: String) = new Agent(s)
    def same = this
    
    /**
     * Get the name without the angle brackets (if any).
     */
    def withoutBrackets: String = {
      val strip1 = name.dropWhile(_ == '<')
      if (strip1.endsWith(">")) strip1.init else strip1
    }

    /**
     * Get the agent with an unqualified version of the agent name, which means
     * without all the rig position information.
     
    def unqualified: Agent =
      if (qualifier == "") this
      else Agent("<" + baseName + ">")
     */

    /**
     * True if this agent is a rig
     */
    def isRig: Boolean = Agent.isRig(name)
  }
  
  /**
   * Methods for testing strings for agent properties
   */
  object Agent {
    /**
     * True if the name is a well-formed agent name,
     * including the angle brackets.
     */
    def isAgent(name: String) = Pattern.matches("<([^>]*:)?([^>]*)>", name)

    /**
     * True if this string is an agent that's a rig
     */
    def isRig(name: String): Boolean = Pattern.matches("<([^>]*:)?rig\\d+>", name)
  }

  implicit def String2AgentName(s: String): Agent = new Agent(s)

  /**
   * A port ID, which consists of the agent name and either the
   * node ID or the cname. Formats will be:
   * `<name1>#12.34.45` or `<name1> beat bar input`.
   * @throws IllegalArgumentException  If the agent and/or label cannot be extracted.
   */
  case class PortID(id: String) extends AgentOrPortID {
    import PortID._

    val str = id
    type T = PortID
    def make(s: String) = new PortID(s)
    def same = this

    private val PortIDRE = """(<[^>]*>)([# ])(.+)""".r

    private val PortIDRE(agent0, sep0, label0) = id

    /**
     * Get the agent name, including the angle brackets.
     */
    val agent: Agent = agent0

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

    /**
     * Get the port ID with an unqualified version of the agent name, which means
     * without all the rig position information.
     
    def unqualified: PortID =
      if (qualifier == "") this
      else PortID("<" + baseName + ">" + nodePart)
     */
      
    /**
     * If this port ID has no explicit qualifier then default it
     * to the one given.
    def defaultQualifier(pos: Pos): PortID =
      if (qualifier == "")
        PortID("<" + pos.qualifier + baseName + ">" + nodePart)
      else
        this
     */

  }

  implicit def string2PortID(id: String) = new PortID(id)

  /**
   * A position in a rig hierarchy in which we're currently
   * interested. An empty sequence means the top level; `("<rig3>")`
   * means rig3 within the top level, and so on.
   */
  case class Pos(p: Agent*) {
    /**
     * Convert a pos to an index specification for the bls command:
     * {{{
     * ()                   => <main>
     * ("<rig1>")           => <main.rig1:main>
     * ("<rig1>", "<rig2>") => <main.rig1:main.rig2:main>
     * }}}
     */
    def index: String = {
      def insertMains(s: Agent) = "." + s.withoutBrackets + ":main"
      "<main" + (p map insertMains).mkString + ">"
    }
    
    /**
     * Append another rig to this pos, to make a new pos one level deeper
     */
    def :+(rig: Agent): Pos = Pos((p.toList :+ rig): _*)

    /**
     * True if this pos represents the top level (length = 0).
     */
    def topLevel: Boolean = p.isEmpty

    /**
     * True if this pos represents something below the top level (length >= 1).
     */
    def notTopLevel: Boolean = p.nonEmpty

    /**
     * Get the parent pos of this one. Will throw an exception if it's
     * the top level pos.
     */
    def parent: Pos = Pos(p.init: _*)

    /**
     * Get the last (lowest-leve) agent in the pos sequence.
     * Will throw an exception if this is the top level pos.
     */
    def last: Agent = p.last
    
    /**
     * The qualifier needed in an agent for this pos.
     * E.g. `List("<rig1>")` yields `"main.rig1:"`
     * and `List()` yields `"main:"`
     */
    def qualifier: String =
      if (p.isEmpty)
        "main:"
      else
        (p map { "main." + _.withoutBrackets + ":" }) mkString

    def displayString: String =
      if (p.isEmpty) "Top level"
      else p.mkString(" - ")

    def length: Int = p.length
  }

  implicit def ListString2Pos(p: List[Agent]): Pos = Pos(p: _*)

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