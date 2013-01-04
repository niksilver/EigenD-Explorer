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

}