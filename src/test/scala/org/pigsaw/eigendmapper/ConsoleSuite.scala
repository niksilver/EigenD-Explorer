package org.pigsaw.eigendmapper

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers

@RunWith(classOf[JUnitRunner])
class ConsoleSuite extends FunSuite with ShouldMatchers {

  test("ConsoleParser - Parses inspect <agent>") {
    val parser = new ConsoleParser
    val cmd: Option[Setup => Setup] = parser.parseLine("inspect <agent>")

    cmd should be('defined)
  }

  test("ConsoleParser - Handles graph nodes") {
    val parser = new ConsoleParser
    val cmd: Option[Setup => Setup] = parser.parseLine("graph nodes")

    cmd should be('defined)
  }
  
  test("ConsoleParser.filePrinter") {
    val cp = new ConsoleParser
    val fp = cp.filePrinter("myfile.txt")
    
    fp.isInstanceOf[FilePrinter] should be (true)
    fp.filename should equal ("myfile.txt")
  }

  test("ConsoleParser - Handles file output") {

    // The command used for testing; it outputs its args but doesn't do
    // anything with the setup it's given
    
    val testCommand = new Command {
      val command = "test"
      def action(args: List[String])(setup: Setup, prln: PrintlnFn): Setup = {
        prln("Args are: " + args)
        setup
      }
    }

    // A FilePrinter that allows us to check how it's used
    
    var filePrinterArg = "Not yet called"
    var filePrinterOutput = ""
    var filePrinterClosed = false

    class TestableFilePrinter(filename: String) extends FilePrinter(filename) {
      filePrinterArg = filename
      override def println(msg: Any) {
        filePrinterOutput = filePrinterOutput + msg
      }
      override def close {
        filePrinterClosed = true
      }
    }

    // A parser which parses the test command
    // uses a testable FilePrinter
    
    val parser = new ConsoleParser {
      override val commands = List(testCommand)
      override def filePrinter(filename: String): FilePrinter =
        new TestableFilePrinter(filename)
    }

    val cmd: Option[Setup => Setup] = parser.parseLine("test output > myfile.txt")

    // The command should be defined
    cmd should be('defined)
    
    // Now run the command (it uses a dummy setup)...
    (cmd.get)(Setup())
    
    // ...and check the right points were hit
    
    filePrinterArg should equal ("myfile.txt")
    filePrinterOutput should equal ("Args are: List(output)")
    filePrinterClosed should equal (true)
    
  }

}