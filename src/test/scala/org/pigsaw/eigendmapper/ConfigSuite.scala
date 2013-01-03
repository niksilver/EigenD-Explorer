package org.pigsaw.eigendmapper

import com.typesafe.config.ConfigFactory

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers

@RunWith(classOf[JUnitRunner])
class ConfigSuite extends FunSuite with ShouldMatchers {

  test("Has readable config") {
    Config.consoleCols should equal (Some(80))
    Config.doesNotExist should equal (None)
    
    Config.bin should be ('defined)
    Config.bin.get(0) should include ("Program Files (x86)\\Eigenlabs")
    Config.bin.get(1) should include ("Program Files\\Eigenlabs")
  }

}