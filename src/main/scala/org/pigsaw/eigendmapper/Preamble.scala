package org.pigsaw.eigendmapper

object Preamble {

  /**
   * Calculate a value, then do something with it before returning it.
   */
  class ReturnableAfter[A](a: A) {
    def returnedAfter(fn: A => Unit): A = { fn(a); a }
  }
  implicit def Any2ReturnableAfter[A](a: A) = new ReturnableAfter(a)

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
     * Get the fully qualified name of this agent at the given position.
     * "<ag1>" + List()                   => <ag1>
     * "<ag1>" + List("<rig1>")           => <main.rig1:ag1>
     * "<ag1>" + List("<rig1>", "<rig2>") => <main.rig1:main.rig2:ag1>
     */
    def fqName(pos: List[String]): String = {
      val mains = pos map { "main." + _.withoutBrackets + ":" }
      "<" + mains.mkString + name.withoutBrackets + ">"
    }

    /**
     * Get the unqualified version of the agent name, which means
     * without all the rig position information.
     */
    def unqualified: String = {
      val FullyQualifiedName = """<.*:(.*)>""".r
      name match {
        case FullyQualifiedName(shortName) => "<" + shortName + ">"
        case _ => name
      }
    }
  }

  implicit def String2AgentName(s: String): AgentName = new AgentName(s)
  
  /**
   * A port ID, which consists of the agent name and either the
   * node ID or the cname. Formats will be:
   * `<name1>#12.34.45` or `<name1> beat bar input`. 
   */
  case class PortID(id: String) {
    import PortID._
    
    /**
     * Get the agent name, including the angle brackets.
     * @throws IllegalArgumentException  If the agent cannot be extracted.
     */
    val agent: String = id match {
      case PortIDRE(agent, _, _) => agent
      case _ => throw new IllegalArgumentException("Cannot parse PortID '" + id + "'")
    }

    /**
     * Get the port ID with an unqualified version of the agent name, which means
     * without all the rig position information.
     */
    def unqualified: String = id match {
      case PortIDRE(agent, sep, label) => agent.unqualified + sep + label
      case _ => throw new IllegalArgumentException("Cannot parse PortID '" + id + "'")
    }
  }
  
  object PortID {
    private val PortIDRE = """(<[^>]*>)([# ])(.+)""".r
  }

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

    def displayString: String =
      if (p.isEmpty) "Top level"
      else p.mkString(" - ")

  }

  implicit def ListString2Pos(p: List[String]): Pos = Pos(p: _*)
}