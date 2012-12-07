package org.pigsaw.eigendmapper

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers

@RunWith(classOf[JUnitRunner])
class CommandsSuite extends FunSuite with ShouldMatchers {
  
  class PrintCatcher(var output: String = "") {
  	val printer = new ShowCommand.Printer({ s => output = output + s.toString + "\n" })
  }

  test("Show - handles no agents") {
    val setup = Setup(Set())
    
    val catcher = new PrintCatcher
    
    ShowCommand.action(List("<rig1>"))(setup)(catcher.printer)
    
    catcher.output should not include ("Unknown")
    catcher.output should include ("No agent called <rig1>")
  }

  test("Show - Produces somewhat sensible output") {
    val conn = Connection(Port("<ttt>#3.3", Some("three")), Port("<fff>#5.5", Some("five")))
    val setup = Setup(Set(conn))
    
    var output = ""
    def println(s: Any): Unit = { output += s.toString + "\n" }
    val printer = new ShowCommand.Printer(println)
    
    ShowCommand.action(List("<ttt>"))(setup)(printer)
    
    output should not include ("Unknown")
    output should include ("-->")
  }
}