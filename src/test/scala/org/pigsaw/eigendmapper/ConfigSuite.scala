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
  }

}