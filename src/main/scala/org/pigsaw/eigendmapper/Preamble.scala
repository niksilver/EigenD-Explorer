package org.pigsaw.eigendmapper

object Preamble {
  /**
   * An agent name, including the angle brackets.
   */
  case class AgentName(name: String) {
    /** Remove the start and end angle brackets, if any. */
    def withoutBrackets: String = {
      val strip1 = name.dropWhile(_ == '<')
      if (strip1.last == '>') strip1.init else strip1
    }
    // ("<(.*)>".r findFirstMatchIn name).get.group(1)
  }

  implicit def String2AgentName(name: String): AgentName =
    AgentName(name)

  /**
   * The position in the rig hierarchy of setups.
   * An empty list means the top level; List("&lt;rig3&gt")
   * means rig3 within the top level, and so on.
   */
  case class Pos(p: String*) {

    /**
     * Convert a pos to an index specification for the bls command:
     * Pos() =>         <main>
     * Pos("<rig1>") => <main.rig1:main>
     * Pos("<rig2>") => <main.rig1:main.rig2:main>
     */
    def index: String = {
      def insertMains(s: String) = "." + s.withoutBrackets + ":main"
      "<main" + (p map insertMains).mkString + ">"
    }
  }
  
  implicit def ListString2Pos(p: List[String]): Pos = Pos(p: _*)
}