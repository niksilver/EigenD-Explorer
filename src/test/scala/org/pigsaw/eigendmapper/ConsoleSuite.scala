/*
 *  Copyright 2012 Nik Silver.
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

package org.pigsaw.eigendmapper

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers

@RunWith(classOf[JUnitRunner])
class ConsoleSuite extends FunSuite with ShouldMatchers {

  trait TestParser extends ConsoleParser {
    def parseOption[T](parser: Parser[T], line: String): Option[T] =
      parseAll(parser, line) match {
        case Success(out, _) => Some(out)
        case fail => None
      }
  }

  test("ConsoleParser.redirect") {
    new TestParser {
      
      // When there are no spaces in the filename

      parseOption(redirect, "> myfile.txt") should equal (Some("myfile.txt"))
      parseOption(redirect, "> 'myfile.txt'") should equal (Some("myfile.txt"))
      parseOption(redirect, """> "myfile.txt"""") should equal (Some("myfile.txt"))
      
      // When we have a space in the filename

      parseOption(redirect, "> my file.txt") should equal (None)
      parseOption(redirect, "> 'my file.txt'") should equal (Some("my file.txt"))
      parseOption(redirect, """> "my file.txt"""") should equal (Some("my file.txt"))

      // We should be able to do without a space after the > sign...
      
      // No space after > and no spaces in the filename

      parseOption(redirect, ">myfile.txt") should equal (Some("myfile.txt"))
      parseOption(redirect, ">'myfile.txt'") should equal (Some("myfile.txt"))
      parseOption(redirect, """>"myfile.txt"""") should equal (Some("myfile.txt"))
      
      // No space after > and we have a space in the filename

      parseOption(redirect, ">my file.txt") should equal (None)
      parseOption(redirect, ">'my file.txt'") should equal (Some("my file.txt"))
      parseOption(redirect, """>"my file.txt"""") should equal (Some("my file.txt"))
    }
  }
  
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