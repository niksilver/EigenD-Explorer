/*
 *  Copyright 2012, 2013 Nik Silver.
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

import java.io.FileWriter
import scala.util.parsing.combinator.RegexParsers

object Console {

  def main(args: Array[String]) {
    val parser = new ConsoleParser
    val ul = new UserLine(">> ")

    outputConfig

    if (EigenD.bin.nonEmpty && Config.consoleCols.nonEmpty) {
      println("There is a help command")
      actLoop(Setup())
    } else
      println("Exiting. Please make corrections in application.conf")

    def actLoop(setup: Setup): Unit = {
      ul.line match {
        case None => ; // Do nothing and hence exit
        case Some(line) => {
          val setup2 = act(line, setup)
          actLoop(setup2)
        }
      }
    }

    def act(line: String, setup: Setup): Setup = {
      parser.parseLine(line) match {
        case Some(command) => command(setup)
        case None => println("Unknown command"); setup
      }
    }
  }

  private def outputConfig {
    Config.consoleCols match {
      case Some(cols) => println("Working to " + cols + " console columns")
      case None => println("Error: Number of console columns not set")
    }
    EigenD.bin match {
      case Some(dir) => println("Found EigenD bin folder " + dir)
      case None => println("Error: No EigenD bin folder found")
    }
  }

}

class ConsoleParser extends RegexParsers {
  override type Elem = Char

  /**
   * Default println function with one argument; may be overriden.
   */
  val prln: Any => Unit = scala.Console.println

  /**
   * Printer to a named file
   */
  def filePrinter(filename: String): FilePrinter =
    new FilePrinter(filename)

  val commands = List(
    new DumpCommand,
    new GraphCommand,
    new HelpCommand,
    new InspectCommand,
    new IntoCommand,
    new SnapshotCommand,
    new UpCommand)

  // A parser for a single command. It outputs a parser which has already
  // been fed the additional arguments, and just needs a setup to process.
  def oneCommandParser(cmd: Command): Parser[(Setup) => Setup] =
    cmd.command ~> (arg *) ~ (redirect ?) ^^ {
      case args ~ Some(filename) => { (s: Setup) => actionToFilePrinter(cmd, args, s, filename) }
      case args ~ None => { (s: Setup) => cmd.action(args)(s, prln) }
    }

  // An argument following the main command. There may be several of these
  // making up the command's arguments. An arg cannot start with a > because
  // that indicates file redirection
  def arg = """[^>]\S*""".r

  def command = someCommand | noCommand

  // In case the user didn't enter anything
  def noCommand = "" ^^ { _ => ((s: Setup) => s) }

  // A parser for any possible command. We write it like this, as a function,
  // rather than writing one production for every command.
  def someCommand = commandsParser(commands)

  // A parser of any number of commands, but at least one. The final parser (if all known
  // commands fail) is one which reports an unknown command
  def commandsParser(cmds: List[Command]): Parser[(Setup) => Setup] = cmds match {
    case Nil => throw new Exception("Must have at least one command")
    case cmd :: Nil => oneCommandParser(cmd)
    case cmd :: tail => oneCommandParser(cmd) | commandsParser(tail)
  }

  // File redirection
  def redirect = ">" ~> filename ^^ { _.mkString }
  def filename = singleQuotedFilename | doubleQuotedFilename | unquotedFilename
  def unquotedFilename = """\S+""".r
  def singleQuotedFilename = "'" ~> "[^']+".r <~ "'" ^^ { _.mkString }
  def doubleQuotedFilename = "\"" ~> "[^\"]+".r <~ "\"" ^^ { _.mkString }

  def parseLine(line: String): Option[(Setup) => Setup] =
    parseAll(phrase(command), line) match {
      case Success(out, _) => Some(out)
      case Failure(msg, _) => None
    }

  /**
   * Execute a given command's action, but redirecting the output
   * to a named file.
   */
  def actionToFilePrinter(cmd: Command, args: List[String], s: Setup, filename: String): Setup = {
    val fp = filePrinter(filename)
    try {
      // This is the returned value
      cmd.action(args)(s, fp.println)
    } finally {
      fp.close
    }
  }
}

/**
 * A printer to a named file.
 */
class FilePrinter(val filename: String) {
  val out = new FileWriter(filename)

  def println(msg: Any) {
    out write msg.toString + "\n"
  }

  def close = out.close
}