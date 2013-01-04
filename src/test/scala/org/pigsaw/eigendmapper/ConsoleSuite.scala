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
    
    cmd should be ('defined)
  }
  
  test("ConsoleParser - Handles graph nodes") {
    val parser = new ConsoleParser
    val cmd: Option[Setup => Setup] = parser.parseLine("graph nodes")
    
    cmd should be ('defined)
  }
  
  test("ConsoleParser - Handles file output") {
    // A setup which captures how it was created.
    class TestableSetup(
        val args: List[String],
        val prln: (Any)=>Unit)
      extends Setup(Set())
    
    // The command which generates a setup showing how it was created
    val testCommand = new Command {
      val command = "test"
      def action(args: List[String])(setup: Setup, prln: PrintlnFn): Setup = {
        new TestableSetup(args, prln)
      }
    }
    
    var filePrinterArg = "Not yet called"
    // A parser which parses the test command
    val parser = new ConsoleParser {
      override val commands = List(testCommand)
      override def filePrinter(filename: String): FilePrinter = {
        filePrinterArg = filename
        new FilePrinter(filename)
      }
    }
    
    val cmd: Option[Setup => Setup] = parser.parseLine("test output > myfile.txt")
    
    cmd should be ('defined)
    val tcmd = (cmd.get)(Setup()).asInstanceOf[TestableSetup]
    tcmd.args should equal (List("output"))
    filePrinterArg should be ("myfile.txt")
  }

}