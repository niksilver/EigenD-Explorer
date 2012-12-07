package org.pigsaw.eigendmapper

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers

@RunWith(classOf[JUnitRunner])
class ConsoleSuite extends FunSuite with ShouldMatchers {
  
  test("ConsoleParser - Parses show <agent>") {
    val parser = new ConsoleParser
    val cmd: Option[Setup => Setup] = parser.parseLine("show <agent>")
    
    cmd should be ('defined)
  }

}