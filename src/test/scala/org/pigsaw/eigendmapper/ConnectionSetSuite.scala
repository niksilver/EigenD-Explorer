package org.pigsaw.eigendmapper

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])  
class ConnectionSetSuite extends FunSuite {

  test("Starts empty") {
    val connSet = ConnectionSet()
    assert(connSet.size === 0)
  }
}