package org.pigsaw.eigendmapper

import com.typesafe.config.ConfigFactory

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers

@RunWith(classOf[JUnitRunner])
class ConfigSuite extends FunSuite with ShouldMatchers {

  test("Has appropriate config") {
    Config.consoleCols should equal (Some(120))
    Config.doesNotExist should equal (None)
    
    Config.eigenDBin should be ('defined)
    Config.eigenDBin.get(0) should include ("Program Files/Eigenlabs")
    Config.eigenDBin.get(1) should include ("Program Files (x86)/Eigenlabs")
  }

}