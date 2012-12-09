package org.pigsaw.eigendmapper

import java.io.FileWriter
import scala.util.parsing.combinator.RegexParsers

object Console {

  def main(args: Array[String]) {
    val parser = new ConsoleParser
    val ul = new UserLine(">> ")
    actLoop(SetupWithPos())

    def actLoop(setup: SetupWithPos): Unit = {
      ul.line match {
        case None => ; // Do nothing and hence exit
        case Some(line) => {
          val setup2 = act(line, setup)
          actLoop(setup2)
        }
      }
    }

    def act(line: String, setup: SetupWithPos): SetupWithPos = {
      parser.parseLine(line) match {
        case Some(command) => command(setup)
        case None => println("Unknown command"); setup
      }
    }
  }

}

class ConsoleParser extends RegexParsers {
  override type Elem = Char

  /**
   * Default println function with one argument; may be overriden.
   */
  val prln: Any=>Unit = scala.Console.println
  
  val commands = List(
      SnapshotCommand,
      ShowCommand,
      GraphCommand,
      HelpCommand)

  // A parser for a single command. It outputs a parser which has already
  // been fed the additional arguments, and just needs a setup to process.
  def oneCommandParser(cmd: Command): Parser[(SetupWithPos) => SetupWithPos] =
    cmd.command ~> (word *) ^^ { case words => ((s: SetupWithPos) => cmd.action(words)(s, prln)) }

  // A word following the main command. There may be several of these
  // making up the command's arguments.
  def word = """\S+""".r

  def command = someCommand | noCommand
  
  // In case the user didn't enter anything
  def noCommand = "" ^^ { _ => ((s: SetupWithPos) => s) }

  // A parser for any possible command
  def someCommand = commandsParser(commands)

  // A parser of any number of commands, but at least one. The final parser (if all known
  // commands fail) is one which reports an unknown command
  def commandsParser(cmds: List[Command]): Parser[(SetupWithPos) => SetupWithPos] = cmds match {
    case Nil         => throw new Exception("Must have at least one command")
    case cmd :: Nil  => oneCommandParser(cmd)
    case cmd :: tail => oneCommandParser(cmd) | commandsParser(tail)
  }

  def parseLine(line: String): Option[(SetupWithPos) => SetupWithPos] =
    parseAll(phrase(command), line) match {
      case Success(out, _) => Some(out)
      case Failure(msg, _) => None
    }

}
