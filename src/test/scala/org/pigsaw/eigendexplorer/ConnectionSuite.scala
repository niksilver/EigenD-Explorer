/*
 *  Copyright 2012 Nik Silver.
 *  
 *  This file is part of EigenD Explorer.
 *
 *  EigenD Explorer is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  EigenD Explorer is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with EigenD Explorer.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.pigsaw.eigendexplorer

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers

@RunWith(classOf[JUnitRunner])
class ConnectionSuite extends FunSuite with ShouldMatchers {

  test("Unqualified - general") {
    val a = "<a>#1.1"
    val a_long = "<main:a>#1.1"
    val b = "<b>#1.2"
    val b_long = "<main:b>#1.2"
    
    Connection(a, b).unqualified should equal (Connection(a, b))
    Connection(a_long, b).unqualified should equal (Connection(a, b))
    Connection(a, b_long).unqualified should equal (Connection(a, b))
    Connection(a_long, b_long).unqualified should equal (Connection(a, b))
  }

  test("Unqualified - object preservation") {
    val a = "<a>#1.1"
    val b = "<b>#1.2"
    
    val a_b = Connection(a, b)

    assert(a_b.unqualified eq a_b)
  }
  
  test("Default qualifier") {
    val conn1Unqual = Connection("<a>#3.2", "<b>#4.5")
    val conn1Qual = Connection("<main:a>#3.2", "<main:b>#4.5")
    conn1Unqual.defaultQualifier(List()) should equal (conn1Qual)
    
    val conn2Qual = Connection("<main.rig1:a>#3.2", "<main.rig1:b>#4.5")
    conn2Qual.defaultQualifier(List()) should equal (conn2Qual)
    
    val conn3SemiQual = Connection("<a>#3.2", "<main.rig1:b>#4.5")
    val conn3Qual = Connection("<main:a>#3.2", "<main.rig1:b>#4.5")
    conn3SemiQual.defaultQualifier(List()) should equal (conn3Qual)
  }
  
  test("Agents") {
    val a = "<a>#1.1"
    val b = "<b>#1.2"
    
    Connection(a, b).agents should equal (Set("<a>", "<b>"))
  }
  
  test("Connection.hasPos") {
    Connection("<a>#3.2", "<b> beat input").hasPos(List()) should equal (true)
    Connection("<a>#3.2", "<b> beat input").hasPos(List("<rig1>")) should equal (false)
    
    Connection("<main.rig1:a>#3.2", "<b> beat input").hasPos(List()) should equal (true)
    Connection("<main.rig1:a>#3.2", "<b> beat input").hasPos(List("<rig1>")) should equal (true)
    Connection("<a>#3.2", "<main.rig1:b> beat input").hasPos(List("<rig1>")) should equal (true)

    Connection("<main.rig1:a>#3.2", "<main.rig1:b> beat input").hasPos(List()) should equal (false)
  }
}