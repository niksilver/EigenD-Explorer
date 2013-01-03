package org.pigsaw.eigendmapper

import com.typesafe.config.ConfigFactory

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers

@RunWith(classOf[JUnitRunner])
class ConfigSuite extends FunSuite with ShouldMatchers {

  test("Has appropriate config") {
    Config.consoleCols should equal (Some(80))
    Config.doesNotExist should equal (None)
    
    Config.eigenDBbin should be ('defined)
    Config.eigenDBbin.get(1) should include ("Program Files (x86)/Eigenlabs")
    Config.eigenDBbin.get(2) should include ("Program Files/Eigenlabs")
  }

}