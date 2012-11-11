package org.pigsaw.eigendmapper

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