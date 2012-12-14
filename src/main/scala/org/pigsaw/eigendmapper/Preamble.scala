package org.pigsaw.eigendmapper

object Preamble {
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
  }

  implicit def String2AgentName(s: String) = new AgentName(s)

  /**
   * A position in a rig hierarchy in which we're currently
   * interested. An empty list means the top level; List("&lt;rig3&gt")
   * means rig3 within the top level, and so on.
   */
  case class Pos(p: String*) {
    /**
     * Convert a pos to an index specification for the bls command:
     * List()                   => <main>
     * List("<rig1>")           => <main.rig1:main>
     * List("<rig1>", "<rig2>") => <main.rig1:main.rig2:main>
     */
    def index: String = {
      def insertMains(s: String) = "." + s.withoutBrackets + ":main"
      "<main" + (p map insertMains).mkString + ">"
    }
    
    /**
     * Get the fully qualified name of the given agent at this position.
     * List() + "<ag1>"                   => <ag1>
     * List("<rig1>") + "<ag1>"           => <main.rig1:ag1>
     * List("<rig1>", "<rig2>") + "<ag1>" => <main.rig1:main.rig2:ag1>
     */
    def fqName(agent: String): String = {
      val mains = p map { "main." + _.withoutBrackets + ":" }
      "<" + mains.mkString + agent.withoutBrackets + ">"
    }

    def displayString: String =
      if (p.isEmpty) "Top level"
        else p.mkString(" - ")

  }
  
  implicit def ListString2Pos(p: List[String]) = Pos(p: _*)
}