/*
 *  Copyright 2012, 2013 Nik Silver.
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

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import Preamble._

@RunWith(classOf[JUnitRunner])
class MapperSuite extends FunSuite {
  
  test("Filter bls stream - sensible case") {
    val bls = new BLs("<main>") {
      override def text = Stream("one", "<two>", "<three>", "four")
    }
    val output = bls.agents
    assert(output.length === 2)
    assert(output contains Agent("<two>"))
    assert(output contains Agent("<three>"))
  }

  test("Filter bls stream - empty case 1") {
    val bls = new BLs("<main>") {
      override def text = Stream("one", "four")
    }
    val output = bls.agents
    assert(output.length === 0)
  }

  test("Filter bls stream - empty case 2") {
    val bls = new BLs("<main>") {
      override def text = Stream()
    }
    val output = bls.agents
    assert(output.length === 0)
  }
 
  /*test("Real bls output") {
    val bls = new BLs("<main>")
    bls.agents.toList map println
  }*/
 
}
