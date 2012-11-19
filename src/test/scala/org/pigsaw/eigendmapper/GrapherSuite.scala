package org.pigsaw.eigendmapper

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers

import org.pigsaw.eigendmapper.Graphable._

@RunWith(classOf[JUnitRunner])
class GrapherSuite extends FunSuite with ShouldMatchers {

  test("Unify - Add a slave name, expect a slave updated") {
    val port_a_unnamed = Port("<a>#1.1", None)
    val port_b_named = Port("<b>#1.2", Some("b"))
    val port_b_unnamed = Port("<b>#1.2", None)
    val port_c_unnamed = Port("<c>#1.3", None)

    val conn_aubu = Connection(port_a_unnamed, port_b_unnamed)

    val connSet1 = Set(conn_aubu)

    connSet1.size should equal (1)
    connSet1 should contain (conn_aubu)

    val conn_cubn = Connection(port_c_unnamed, port_b_named) // We'll add this
    val conn_aubn = Connection(port_a_unnamed, port_b_named) // This should get created
    
    val connSet2 = (connSet1 + conn_cubn).unified

    connSet2.size should equal (2)
    connSet2 should contain (conn_cubn)
    connSet2 should not contain (conn_aubu)
    connSet2 should contain (conn_aubn)
  }

  test("Unify - Add a slave name, expect a master updated") {
    val port_a_unnamed = Port("<a>#1.1", None)
    val port_b_named = Port("<b>#1.2", Some("b"))
    val port_b_unnamed = Port("<b>#1.2", None)
    val port_c_unnamed = Port("<c>#1.3", None)

    val conn_buau = Connection(port_b_unnamed, port_a_unnamed)

    val connSet1 = Set(conn_buau)

    connSet1.size should equal (1)
    connSet1 should contain (conn_buau)

    val conn_cubn = Connection(port_c_unnamed, port_b_named) // We'll add this
    val conn_bnau = Connection(port_b_named, port_a_unnamed) // This should get created
    
    val connSet2 = (connSet1 + conn_cubn).unified

    connSet2.size should equal (2)
    connSet2 should contain (conn_cubn)
    connSet2 should not contain (conn_buau)
    connSet2 should contain (conn_bnau)
  }

  test("Unify - Add a master name, expect a slave updated") {
    val port_a_unnamed = Port("<a>#1.1", None)
    val port_b_named = Port("<b>#1.2", Some("b"))
    val port_b_unnamed = Port("<b>#1.2", None)
    val port_c_unnamed = Port("<c>#1.3", None)

    val conn_aubu = Connection(port_a_unnamed, port_b_unnamed)

    val connSet1 = Set(conn_aubu)

    connSet1.size should equal (1)
    connSet1 should contain (conn_aubu)

    val conn_bncu = Connection(port_b_named, port_c_unnamed) // We'll add this
    val conn_aubn = Connection(port_a_unnamed, port_b_named) // This should get created
    
    val connSet2 = (connSet1 + conn_bncu).unified

    connSet2.size should equal (2)
    connSet2 should contain (conn_bncu)
    connSet2 should not contain (conn_aubu)
    connSet2 should contain (conn_aubn)
  }

  test("Unify - Add a master name, expect a master updated") {
    val port_a_unnamed = Port("<a>#1.1", None)
    val port_b_named = Port("<b>#1.2", Some("b"))
    val port_b_unnamed = Port("<b>#1.2", None)
    val port_c_unnamed = Port("<c>#1.3", None)

    val conn_buau = Connection(port_b_unnamed, port_a_unnamed)

    val connSet1 = Set(conn_buau)

    connSet1.size should equal (1)
    connSet1 should contain (conn_buau)

    val conn_bncu = Connection(port_b_named, port_c_unnamed) // We'll add this
    val conn_bnau = Connection(port_b_named, port_a_unnamed) // This should get created
    
    val connSet2 = (connSet1 + conn_bncu).unified

    connSet2.size should equal (2)
    connSet2 should contain (conn_bncu)
    connSet2 should not contain (conn_buau)
    connSet2 should contain (conn_bnau)
  }

}