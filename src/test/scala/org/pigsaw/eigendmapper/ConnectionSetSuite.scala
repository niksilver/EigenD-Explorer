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

  test("Can add elements") {
    val connSet0 = ConnectionSet()
    val conn1 = Connection(Port("<a>#1.1", Some("a")), Port("<b>#1.2", None))
    val conn2 = Connection(Port("<c>#1.3", None), Port("<d>#1.4", Some("d")))
    val conn3 = Connection(Port("<e>#1.5", None), Port("<f>#1.6", Some("f")))
    
    val connSet2 = connSet0 + conn1 + conn2
    
    assert(connSet2.size === 2)
    assert(connSet2 contains conn1)
    assert(connSet2 contains conn2)
    assert(!(connSet2 contains conn3))
  }
}