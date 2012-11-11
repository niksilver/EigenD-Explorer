package org.pigsaw.eigendmapper

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.pigsaw.eigendmapper._
import org.pigsaw.eigendmapper.Mapper._

@RunWith(classOf[JUnitRunner])
class DictionarySuite extends FunSuite {

  test("Read state variable") {
    object TestParser extends DictionaryOutputParser {
      parseAll(outputLine, "1 hello") match {
        case Success(lup, _) => println("lup... " + lup)
        case x => println("x... " + x)
      }
    }
    TestParser
  }

}
