package org.pigsaw.eigendmapper

import jline.ConsoleReader

class UserLine(prompt: String) {
  def line: Option[String] = UserLine.reader.readLine(prompt) match {
    case null => None
    case line => Some(line)
  }
}

object UserLine {
  val reader = new ConsoleReader()
}