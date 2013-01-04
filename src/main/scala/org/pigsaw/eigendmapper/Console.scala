package org.pigsaw.eigendmapper

import java.io.FileWriter
import scala.util.parsing.combinator.RegexParsers

object Console {

  def main(args: Array[String]) {
    val parser = new ConsoleParser
    val ul = new UserLine(">> ")

    outputConfig

    if (EigenD.bin.nonEmpty && Config.consoleCols.nonEmpty)
      actLoop(Setup())
    else
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
  def filePrinter(filename: String): FilePrinter = {
    throw new Exception("Not yet implemented")
  }

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
      case args ~ Some(filename) => { (s: Setup) => val fp = filePrinter(filename); val s2 = cmd.action(args)(s, prln); fp.close; s2 }
      case args ~ None => { (s: Setup) => cmd.action(args)(s, prln) }
    }

  // An argument following the main command. There may be several of these
  // making up the command's arguments. An arg cannot start with a > because
  // that indicates file redirection
  def arg = """[^>]\S*""".r

  def command = someCommand | noCommand

  // In case the user didn't enter anything
  def noCommand = "" ^^ { _ => ((s: Setup) => s) }

  // A parser for any possible command
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
  def filename = """\S+""".r

  def parseLine(line: String): Option[(Setup) => Setup] =
    parseAll(phrase(command), line) match {
      case Success(out, _) => Some(out)
      case Failure(msg, _) => None
    }
}

/**
 * A printer to a named file.
 */
class FilePrinter(filename: String) {
  val out = new FileWriter(filename)
  
  def println(msg: String) {
    out write msg
  }
  
  def close = out.close
}