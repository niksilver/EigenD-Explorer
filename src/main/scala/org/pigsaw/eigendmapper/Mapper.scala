package org.pigsaw.eigendmapper

object Mapper {
  def filterAgents(in: Stream[String]): List[String] =
    in filter ( s => s.startsWith("<") && s.endsWith(">") ) map (_.tail.init) toList
}