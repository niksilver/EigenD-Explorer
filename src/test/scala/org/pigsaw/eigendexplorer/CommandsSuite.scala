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
class CommandsSuite extends FunSuite with ShouldMatchers {

  class PrintCatcher {
    var output = ""
    def println(s: Any): Unit = { output = output + s.toString + "\n" }
  }

  // -------------------------------------------------------------------------------
  //
  // Graph
  //
  // -------------------------------------------------------------------------------

  test("Graph - handles no arguments") {
    val setup = Setup()

    val catcher = new PrintCatcher
    val args = List()

    (new GraphCommand).action(args)(setup, catcher.println)

    catcher.output should include("You need to specify")
  }

  test("Graph - handles too many args") {
    val setup = Setup()

    val catcher = new PrintCatcher
    val args = List("a", "b")

    (new GraphCommand).action(args)(setup, catcher.println)

    catcher.output should include("Too many arguments")
  }

  test("Graph - handles single bad argument") {
    val setup = Setup()

    val catcher = new PrintCatcher
    val args = List("wrong")

    (new GraphCommand).action(args)(setup, catcher.println)

    catcher.output should include("Do not recognise what to graph")
  }

  test("GraphCommand.portPortConns - Basic test") {
    val a1 = "<main:a>#1.1"
    val a2 = "<main:a>#1.2"
    val b1 = "<main:b>#1.1"
    val b2 = "<main:b>#1.2"
    val conns = Set(
      Connection(a1, b1),
      Connection(a2, b2))
    val names = Map(
        "<main:a>#1.1" -> "<main:a> a one one",
        "<main:b>#1.1" -> "<main:b> b one one")

    val setup = Setup().withConns(conns).withPortNames(names)

    val cmd = new GraphCommand
    val conns2 = cmd.portPortConns(setup)

    conns2.size should equal (2)
    conns2 should contain (Connection("<a> a one one", "<b> b one one"))
    conns2 should contain (Connection("<a>#1.2", "<b>#1.2"))
  }

  test("GraphCommand.portPortConns - Only outputs current rig") {
    val a1 = "<main:a>#1.1"
    val a2 = "<main.rig3:a>#1.2"
    val b1 = "<main:b>#1.1"
    val b2 = "<main.rig3:b>#1.2"
    val conns = Set(
      Connection(a1, b1),
      Connection(a2, b2))
    val names = Map(
        "<main:a>#1.1" -> "<main:a> a one one",
        "<main:b>#1.1" -> "<main:b> b one one")
    val pos = List("<rig3>")

    val setup = Setup().withConns(conns).withPortNames(names).withPosUpdated(pos)

    val cmd = new GraphCommand
    val conns2 = cmd.portPortConns(setup)

    conns2.size should equal (1)
    conns2 should contain (Connection("<a>#1.2", "<b>#1.2"))
  }

  test("GraphCommand.portPortConns - Includes any if in current rig") {
    val a1 = "<main:a>#1.1"
    val a2 = "<main.rig3:a>#1.2"
    val b1 = "<main:b>#1.1"
    val b2 = "<main.rig8:b>#1.2"
    val conns = Set(
      Connection(a1, b1),
      Connection(a2, b2))
    val names = Map(
        "<main:a>#1.1" -> "<main:a> a one one",
        "<main:b>#1.1" -> "<main:b> b one one")
    val pos = List("<rig3>")

    val setup = Setup().withConns(conns).withPortNames(names).withPosUpdated(pos)

    val cmd = new GraphCommand
    val conns2 = cmd.portPortConns(setup)

    conns2.size should equal (1)
    conns2 should contain (Connection("<a>#1.2", "<main.rig8:b>#1.2"))
  }

  test("GraphCommand.agentPortConns - Basic test") {
    val a1 = "<main:a>#1.1"
    val a2 = "<main:a>#1.2"
    val b1 = "<main:b>#1.1"
    val b2 = "<main:b>#1.2"
    val conns = Set(
      Connection(a1, b1),
      Connection(a2, b2))
    val names = Map(
        "<main:a>#1.1" -> "<main:a> a one one",
        "<main:b>#1.1" -> "<main:b> b one one")

    val setup = Setup().withConns(conns).withPortNames(names)

    val cmd = new GraphCommand
    val conns2 = cmd.agentPortConns(setup)

    conns2.size should equal(2)
    conns2 should contain("<a>" -> "<a> a one one")
    conns2 should contain("<a>" -> "<a>#1.2")
  }

  test("GraphCommand.agentPortConns - Only outputs current rig") {
    val a1 = "<main:a>#1.1"
    val a2 = "<main.rig3:a>#1.2"
    val b1 = "<main:b>#1.1"
    val b2 = "<main.rig3:b>#1.2"
    val conns = Set(
      Connection(a1, b1),
      Connection(a2, b2))
    val names = Map(
        "<main:a>#1.1" -> "<main:a> a one one",
        "<main:b>#1.1" -> "<main:b> b one one")
    val pos = List("<rig3>")

    val setup = Setup().withConns(conns).withPortNames(names).withPosUpdated(pos)

    val cmd = new GraphCommand
    val conns2 = cmd.agentPortConns(setup)

    conns2.size should equal(1)
    conns2 should contain("<a>" -> "<a>#1.2")
  }

  test("GraphCommand.agentPortConns - Includes agents with ports going into current rig") {
    val a1 = "<main:a>#1.1"
    val b1 = "<main.rig3:b>#1.1"
    val conns = Set(
      Connection(a1, b1))
    val pos = List("<rig3>")

    val setup = Setup().withConns(conns).withPosUpdated(pos)

    val cmd = new GraphCommand
    val conns2 = cmd.agentPortConns(setup)

    conns2.size should equal(1)
    conns2 should contain("<main:a>" -> "<main:a>#1.1")
  }


  test("GraphCommand.portAgentConns - Basic test") {
    val a1 = "<main:a>#1.1"
    val a2 = "<main:a>#1.2"
    val b1 = "<main:b>#1.1"
    val b2 = "<main:b>#1.2"
    val conns = Set(
      Connection(a1, b1),
      Connection(a2, b2))
    val names = Map(
        "<main:a>#1.1" -> "<main:a> a one one",
        "<main:b>#1.1" -> "<main:b> b one one")

    val setup = Setup().withConns(conns).withPortNames(names)

    val cmd = new GraphCommand
    val conns2 = cmd.portAgentConns(setup)

    conns2.size should equal(2)
    conns2 should contain("<b> b one one" -> "<b>")
    conns2 should contain("<b>#1.2" -> "<b>")
  }

  test("GraphCommand.portAgentConns - Only outputs current rig") {
    val a1 = "<main:a>#1.1"
    val a2 = "<main.rig3:a>#1.2"
    val b1 = "<main:b>#1.1"
    val b2 = "<main.rig3:b>#1.2"
    val conns = Set(
      Connection(a1, b1),
      Connection(a2, b2))
    val names = Map(
        "<main:a>#1.1" -> "<main:a> a one one",
        "<main:b>#1.1" -> "<main:b> b one one")
    val pos = List("<rig3>")

    val setup = Setup().withConns(conns).withPortNames(names).withPosUpdated(pos)

    val cmd = new GraphCommand
    val conns2 = cmd.portAgentConns(setup)

    conns2.size should equal(1)
    conns2 should contain("<b>#1.2" -> "<b>")
  }

  test("GraphCommand.portAgentConns - Includes agents with ports coming out of current rig") {
    val a1 = "<main.rig3:a>#1.1"
    val b1 = "<main:b>#1.1"
    val conns = Set(
      Connection(a1, b1))
    val pos = List("<rig3>")

    val setup = Setup().withConns(conns).withPosUpdated(pos)

    val cmd = new GraphCommand
    val conns2 = cmd.portAgentConns(setup)

    conns2.size should equal(1)
    conns2 should contain("<main:b>#1.1" -> "<main:b>")
  }

  test("GraphCommand.agentAgentConns - Basic test") {
    val a1 = "<main:a>#1.1"
    val a2 = "<main:a>#1.2"
    val b1 = "<main:b>#1.1"
    val b2 = "<main:b>#1.2"
    val conns = Set(
      Connection(a1, b1),
      Connection(a2, b2))
    val names = Map(
        "<main:a>#1.1" -> "<main:a> a one one",
        "<main:b>#1.1" -> "<main:b> b one one")

    val setup = Setup().withConns(conns).withPortNames(names)

    val cmd = new GraphCommand
    val conns2 = cmd.agentAgentConns(setup)

    conns2.size should equal (1)
    conns2 should contain (("<a>", "<b>"))
  }
  
  test("GraphCommand.agentAgentConns - Only outputs current rig") {
    val a1 = "<main.rig3:a>#1.1"
    val b1 = "<main:b>#1.1"
    val b2 = "<main:b>#1.2"
    val c2 = "<main:c>#1.2"
    val conns = Set(
      Connection(a1, b1),
      Connection(b2, c2))
    val pos = List("<rig3>")
      
    val setup = Setup().withConns(conns).withPosUpdated(pos)

    val cmd = new GraphCommand
    val conns2 = cmd.agentAgentConns(setup)

    conns2.size should equal (1)
    conns2 should contain (("<a>", "<main:b>"))
  }
  
  // -------------------------------------------------------------------------------
  //
  // Inspect
  //
  // -------------------------------------------------------------------------------

  test("Inspect - handles no agents") {
    val setup = Setup()

    val catcher = new PrintCatcher

    (new InspectCommand).action(List("<rig1>"))(setup, catcher.println)

    catcher.output should not include ("Unknown")
    catcher.output should include("No agent called <rig1>")
  }

  test("Inspect - Produces somewhat sensible output") {
    val conn = Connection("<ttt> three", "<fff> five")
    val setup = Setup(Set(conn))

    val catcher = new PrintCatcher

    (new InspectCommand).action(List("<ttt>"))(setup, catcher.println)

    catcher.output should not include ("Unknown")
    catcher.output should include("-->")
  }

  test("Inspect - Uses portNames map") {
    val conn1 = Connection("<prev>#3.3", "<curr>#1.1") // Will have names
    val conn2 = Connection("<prev>#3.4", "<curr>#1.2") // Won't have names
    val conn3 = Connection("<curr>#2.2", "<next>#4.4") // Will have names
    val conn4 = Connection("<curr>#2.3", "<next>#4.5") // Won't have names

    val portNames = Map(
      "<prev>#3.3" -> "<prev> three three",
      "<curr>#1.1" -> "<curr> one one",
      "<curr>#2.2" -> "<curr> two two",
      "<next>#4.4" -> "<next> four four")

    val setup = Setup(Set(conn1, conn2, conn3, conn4)).
      withPortNames(portNames)

    val catcher = new PrintCatcher

    (new InspectCommand).action(List("<curr>"))(setup, catcher.println)

    catcher.output should not include ("Unknown")
    catcher.output should include("<prev> three three --> one one")
    catcher.output should include("<prev>#3.4         --> #1.2")
    catcher.output should include("two two --> <next> four four")
    catcher.output should include("#2.3    --> <next>#4.5")
  }

  test("Inspect - Displays settings for ports with connections") {
    val conn1 = Connection("<prev>#3.3", "<curr>#1.1")
    val conn2 = Connection("<prev>#3.4", "<curr>#1.2")

    val settings = Map(
      "<prev>#3.3" -> "three three", // Should not show, as not current agent
      "<curr>#1.2" -> "one two")

    val setup = Setup(Set(conn1, conn2)).
      withSettings(settings)

    val catcher = new PrintCatcher

    (new InspectCommand).action(List("<curr>"))(setup, catcher.println)

    catcher.output should not include ("Unknown")
    catcher.output should include("<prev>#3.3 --> #1.1")
    catcher.output should include("<prev>#3.4 --> #1.2 = one two")
  }

  test("Inspect - Displays settings for ports with no connections") {
    val conn1 = Connection("<prev>#3.3", "<curr>#1.1")
    val conn2 = Connection("<prev>#3.4", "<curr>#1.2")

    val settings = Map(
      "<curr>#1.3" -> "one three")

    val setup = Setup(Set(conn1, conn2)).
      withSettings(settings)

    val catcher = new PrintCatcher

    (new InspectCommand).action(List("<curr>"))(setup, catcher.println)

    catcher.output should not include ("Unknown")
    catcher.output should include("#1.3 = one three")
  }

  test("Inspect - Omits unlinked settings which are the empty string") {
    val conn1 = Connection("<prev>#3.3", "<curr>#1.1")
    val conn2 = Connection("<prev>#3.4", "<curr>#1.2")

    val settings = Map(
      "<curr>#1.3" -> "")

    val setup = Setup(Set(conn1, conn2)).
      withSettings(settings)

    val catcher = new PrintCatcher

    (new InspectCommand).action(List("<curr>"))(setup, catcher.println)

    catcher.output should not include ("Unknown")
    catcher.output should not include ("#1.3")
  }

  test("Inspect - Omits linked settings which are the empty string") {
    val conn1 = Connection("<prev>#3.3", "<curr>#1.1")
    val conn2 = Connection("<prev>#3.4", "<curr>#1.2")

    val settings = Map(
      "<curr>#1.1" -> "")

    val setup = Setup(Set(conn1, conn2)).
      withSettings(settings)

    val catcher = new PrintCatcher

    (new InspectCommand).action(List("<curr>"))(setup, catcher.println)

    catcher.output should not include ("Unknown")
    catcher.output should include("--> #1.1")
    catcher.output should not include ("--> #1.1 =")
  }

  test("Inspect - Omits linked and unlinked settings which are XML") {
    val conn1 = Connection("<prev>#3.3", "<curr>#1.1")
    val conn2 = Connection("<prev>#3.4", "<curr>#1.2")

    val settings = Map(
      "<curr>#1.2" -> "<widget name='my first widget'/>",
      "<curr>#2.2" -> "<widget name='my second widget'/>")

    val setup = Setup(Set(conn1, conn2)).
      withSettings(settings)

    val catcher = new PrintCatcher

    (new InspectCommand).action(List("<curr>"))(setup, catcher.println)

    catcher.output should not include ("Unknown")
    catcher.output should include("--> #1.2")
    catcher.output should not include ("--> #1.2 =")
    catcher.output should not include ("#2.2")
  }

  test("Inspect - Truncates decimal setting values to 3dp") {
    val conn1 = Connection("<prev>#3.3", "<curr>#1.1")
    val conn2 = Connection("<prev>#3.4", "<curr>#1.2")

    val settings = Map(
      "<curr>#1.1" -> "34.1234",
      "<curr>#1.2" -> "-34.1234")

    val setup = Setup(Set(conn1, conn2)).
      withSettings(settings)

    val catcher = new PrintCatcher

    (new InspectCommand).action(List("<curr>"))(setup, catcher.println)

    catcher.output should not include ("Unknown")
    catcher.output should include("--> #1.1 = 34.123")
    catcher.output should not include ("--> #1.1 = 34.1234")
    catcher.output should include("--> #1.2 = -34.123")
    catcher.output should not include ("--> #1.2 = -34.1234")
  }

  test("Inspect - Handles being in a rig") {
    val connTop = Connection("<rig1> one", "<fff> five")
    val connRig = Connection("<main.rig1:aaa> ayes", "<main.rig1:bbb> bees")

    val setupTop = Setup(Set(connTop, connRig)).withPosUpdated(List("<rig1>"))

    val catcher = new PrintCatcher

    (new InspectCommand).action(List("<aaa>"))(setupTop, catcher.println)

    catcher.output should not include ("Unknown")
    catcher.output should not include ("No agent called")
    catcher.output should include("ayes --> <bbb> bees")
  }

  test("Inspect - Allows qualified agent name") {
    val connTop = Connection("<rig1>#1.1", "<fff>#5.5")
    val connRig = Connection("<main.rig1:aaa>#2.2", "<main.rig1:bbb>#2.3")

    val setupTop = Setup(Set(connTop, connRig)).withPosUpdated(List("<rig1>"))

    val catcher = new PrintCatcher

    (new InspectCommand).action(List("<main:fff>"))(setupTop, catcher.println)

    catcher.output should not include ("Unknown")
    catcher.output should not include ("No agent called")
    catcher.output should include("<main:rig1>#1.1 --> #5.5")
  }

  test("Inspect - Catches malformed agent name") {
    val setup = Setup()
    val catcher = new PrintCatcher

    (new InspectCommand).action(List("<fff"))(setup, catcher.println)

    catcher.output should not include ("Unknown")
    catcher.output should not include ("No agent called")
    catcher.output should include("Bad agent name")
  }

  test("Inspect - Doesn't show different agent with same name in other rig") {
    val connTop = Connection("<rig1> one", "<aaa> top aye")
    val connRig = Connection("<main.rig1:aaa> ayes", "<main.rig1:bbb> bees")

    val setupTop = Setup(Set(connTop, connRig)).withPosUpdated(List("<rig1>"))

    val catcher = new PrintCatcher

    (new InspectCommand).action(List("<aaa>"))(setupTop, catcher.println)

    catcher.output should not include ("Unknown")
    catcher.output should not include ("No agent called")
    catcher.output should include("ayes --> <bbb> bees")
    catcher.output should not include ("top aye")
  }

  test("Inspect - Omits unlinked settings from other agents") {
    val conn1 = Connection("<prev>#3.3", "<curr>#1.1")
    val conn2 = Connection("<prev>#3.4", "<curr>#1.2")

    val settings = Map(
      "<other>#5.5" -> "some value")

    val setup = Setup(Set(conn1, conn2)).
      withSettings(settings)

    val catcher = new PrintCatcher

    (new InspectCommand).action(List("<curr>"))(setup, catcher.println)

    catcher.output should not include ("Unknown")
    catcher.output should include("--> #1.1")
    catcher.output should include("--> #1.2")
    catcher.output should not include ("#5.5")
  }

  // -------------------------------------------------------------------------------
  //
  // Into
  //
  // -------------------------------------------------------------------------------

  test("Into - Can go into an empty rig") {
    val connsTop = Connection("<rig1> one out", "<top> top input")
    val connsMid = Connection("<main.rig1:rig2> two out", "<main.rig1:mid> mid input")
    val connsBottom = Connection("<main.rig1:main.rig2:rig3> three out", "<main.rig1:main.rig2:bottom> bottom input")

    val setupTop = Setup(Set(connsTop, connsMid, connsBottom)).withPosUpdated(List("<rig1>", "<rig2>"))

    val command = new IntoCommand
    val catcher = new PrintCatcher
    val setupTop2 = command.action(List("<rig3>"))(setupTop, catcher.println)

    setupTop2.pos should equal(List("<rig1>", "<rig2>", "<rig3>"))
    catcher.output should include("Position: <rig1> - <rig2> - <rig3>")
  }

  test("Into - Can go into an already-present rig") {
    val connsTop = Connection("<rig1> one out", "<top> top input")
    val connsMid = Connection("<main.rig1:rig2> two out", "<main.rig1:mid> mid input")
    val connsBottom = Connection("<main.rig1:main.rig2:free> three out", "<main.rig1:main.rig2:bottom> bottom input")

    val setupTop = Setup(Set(connsTop, connsMid, connsBottom)).withPosUpdated(List("<rig1>"))

    val command = new IntoCommand
    val catcher = new PrintCatcher
    val setupTop2 = command.action(List("<rig2>"))(setupTop, catcher.println)

    setupTop2.pos should equal(List("<rig1>", "<rig2>"))
    catcher.output should include("Position: <rig1> - <rig2>")
  }

  test("Into - Can't go into a non-existent rig") {
    val connsTop = Connection("<rig1> one out", "<top> top input")
    val connsMid = Connection("<main.rig1:rig2> two out", "<main.rig1:mid> mid input")
    val connsBottom = Connection("<main.rig1:main.rig2:free> three out", "<main.rig1:main.rig2:bottom> bottom input")

    val setupTop = Setup(Set(connsTop, connsMid, connsBottom)).withPosUpdated(List("<rig1>"))

    val command = new IntoCommand
    val catcher = new PrintCatcher
    val setupTop2 = command.action(List("<rig77>"))(setupTop, catcher.println)

    setupTop2.pos should equal(List("<rig1>"))
    catcher.output.lines.toList(0) should equal("No such rig: <rig77>")
    catcher.output.lines.toList(1) should equal("Position: <rig1>")
  }

  test("Into - Can't go into non-existent rig at top level ") {
    val connsTop = Connection("<rig1> one out", "<top> top input")
    val connsMid = Connection("<main.rig1:rig2> two out", "<main.rig1:mid> mid input")
    val connsBottom = Connection("<main.rig1:main.rig2:free> three out", "<main.rig1:main.rig2:bottom> bottom input")

    val setupTop = Setup(Set(connsTop, connsMid, connsBottom))

    val command = new IntoCommand
    val catcher = new PrintCatcher
    val setupTop2 = command.action(List("<rig77>"))(setupTop, catcher.println)

    setupTop2.pos should equal(List())
    catcher.output.lines.toList(0) should equal("No such rig: <rig77>")
    catcher.output.lines.toList(1) should equal("Position: Top level")
  }

  test("Into - Handles bad arguments") {
    val connsTop = Connection("<rig1> one out", "<top> top input")
    val setupTop = Setup(Set(connsTop))

    val command = new IntoCommand

    val catcher1 = new PrintCatcher
    command.action(List())(setupTop, catcher1.println)
    catcher1.output.lines.toList(0) should equal("into: Too few arguments")
    catcher1.output.lines.toList(1) should equal("Position: Top level")

    val catcher2 = new PrintCatcher
    command.action(List("a", "b"))(setupTop, catcher2.println)
    catcher2.output.lines.toList(0) should equal("into: Too many arguments")
    catcher2.output.lines.toList(1) should equal("Position: Top level")

    val catcher3 = new PrintCatcher
    command.action(List("<top>"))(setupTop, catcher3.println)
    catcher3.output.lines.toList(0) should equal("into: <top> is not a rig")
    catcher3.output.lines.toList(1) should equal("Position: Top level")

    val catcher4 = new PrintCatcher
    command.action(List("<rig1"))(setupTop, catcher4.println)
    catcher4.output.lines.toList(0) should include("into: Bad rig name")
    catcher4.output.lines.toList(1) should equal("Position: Top level")
  }

  // -------------------------------------------------------------------------------
  //
  // Snapshot
  //
  // -------------------------------------------------------------------------------

  test("Snapshot - Correct bls index for top level") {

    val command = new SnapshotCommand {
      var capturedIndex = "not yet set"
      override def bls(index: String): BLs = new BLs(index) {
        capturedIndex = index
        override def text: Stream[String] = Stream("something bls-ish")
      }
      override def bcat(agent: String): BCat = new BCat(agent) {
        override def text: Stream[String] = Stream("something bcatty")
      }
    }

    val catcher = new PrintCatcher

    val setup = Setup()
    val setupV2 = command.action(List())(setup, catcher.println)

    command.capturedIndex should equal("<main>")
  }

  test("Snapshot - Correct FQ names for first level down") {

    val command = new SnapshotCommand {
      var capturedIndex = "not yet set"
      var capturedAgents = collection.mutable.Set[String]()
      override def bls(index: String): BLs = new BLs(index) {
        capturedIndex = index
        override def text: Stream[String] = Stream("<too>", "<mid>")
      }
      override def bcat(agent: String): BCat = new BCat(agent) {
        capturedAgents = capturedAgents + agent
        override def text: Stream[String] = Stream("something bcatty")
      }
    }

    val connsTop = Connection("<rig1> three", "<fff> five")
    val connsRig = Connection("<main.rig1:too> two out", "<main.rig1:mid> mid input")

    val catcher = new PrintCatcher

    val setup = Setup(Set(connsTop, connsRig)).withPosUpdated(List("<rig1>"))
    val setupV2 = command.action(List())(setup, catcher.println)

    command.capturedIndex should equal("<main.rig1:main>")
    command.capturedAgents should equal(Set("<main.rig1:too>", "<main.rig1:mid>"))
  }

  test("Snapshot - Preserves other setup data") {
    val connsTop = Connection("<main:rig1> one out", "<main:top> top input")
    val connsRig = Connection("<main.rig1:too> two out", "<main.rig1:mid> mid input")

    val setupTop = Setup(Set(connsTop, connsRig)).withPosUpdated(List("<rig1>"))

    val connsRigV2 = Connection("<main.rig1:too> two out2", "<main.rig1:mid> mid input2")

    //val connsRigV2Unqual = Connection("<too> two out2", "<mid> mid input2")

    val command = new SnapshotCommand {
      override def bls(index: String): BLs = new BLs(index) {
        override def agents: List[String] = List("<rig1>")
      }
      override def bcat(agent: String): BCat = new BCat(agent) {
        override lazy val connections: Set[Connection] = Set(connsRigV2)
        override def text: Stream[String] = Stream("nothing here")
      }
    }

    val catcher = new PrintCatcher
    val setupTop2 = command.action(List())(setupTop, catcher.println)

    setupTop2.conns(List()) should equal(Set(connsTop))
    setupTop2.rigs(List()) should equal(Set("<rig1>"))
    setupTop2.pos should equal(List("<rig1>"))

    setupTop2.conns should equal(Set(connsRigV2))
  }

  test("Snapshot - Captures port cnames") {

    val command = new SnapshotCommand {
      var capturedIndex = "not yet set"
      var capturedAgents = collection.mutable.Set[String]()
      override def bls(index: String): BLs = new BLs(index) {
        capturedIndex = index
        override def text: Stream[String] = Stream("<ag1>", "<ag2>")
      }
      // In this setup we have:
      // <ag1>#1.1 --> <ag2>#2.1
      // <ag1>#1.2 --> <ag2>#2.2 --> <ag1>#1.22
      // <ag1>#1.3 --> <ag2>#2.3
      // <ag1>#1.4 --> <ag2>#2.4
      // And we have cnames:
      // <ag1>#1.1 = one one
      // <ag2>#2.2 = two two
      // <ag2>#2.3 = two three
      override def bcat(agent: String): BCat = new BCat(agent) {
        capturedAgents = capturedAgents + agent
        val ag1Text = """. {cname:ag1,slave:}
          |1.1 {cname:one one,protocols:input,slave:'<ag2>#2.1'}
          |1.2 {protocols:input,slave:'<ag2>#2.2'}
          |1.3 {protocols:input,slave:'<ag2>#2.3'}
          |1.4 {protocols:input,slave:'<ag2>#2.4'}
          |1.22 {master:conn(None,None,'<ag2>#2.2',None,ctl)}""".stripMargin
        val ag2Text = """. {cname:ag2,slave:}
          |2.1 {master:conn(None,None,'<ag1>#1.1',None,ctl)}
          |2.2 {cname:two two,master:conn(None,None,'<ag1>#1.2',None,ctl)}
          |2.3 {cname:two three,master:conn(None,None,'<ag1>#1.3',None,ctl)}
          |2.4 {master:conn(None,None,'<ag1>#1.4',None,ctl)}""".stripMargin
        override def text: Stream[String] =
          (if (agent == "<main:ag1>") ag1Text else ag2Text).lines.toStream
      }
    }

    val catcher = new PrintCatcher

    val setup = command.action(List())(Setup(), catcher.println)

    setup.conns.size should equal(5)
    val processedPorts = setup.ports map setup.portIDNamed

    processedPorts should contain("<main:ag1> one one")
    processedPorts should contain("<main:ag1>#1.2")
    processedPorts should contain("<main:ag1>#1.3")
    processedPorts should contain("<main:ag1>#1.4")
    processedPorts should contain("<main:ag1>#1.22")

    processedPorts should contain("<main:ag2>#2.1")
    processedPorts should contain("<main:ag2> two two")
    processedPorts should contain("<main:ag2> two three")
    processedPorts should contain("<main:ag2>#2.4")

    val processedConns = setup.conns map { c =>
      Connection(setup.portIDNamed(c.master), setup.portIDNamed(c.slave))
    }

    processedConns should contain(Connection("<main:ag1> one one", "<main:ag2>#2.1"))
    processedConns should contain(Connection("<main:ag1>#1.2", "<main:ag2> two two"))
    processedConns should contain(Connection("<main:ag1>#1.3", "<main:ag2> two three"))
    processedConns should contain(Connection("<main:ag1>#1.4", "<main:ag2>#2.4"))
    processedConns should contain(Connection("<main:ag2> two two", "<main:ag1>#1.22"))
  }

  test("Snapshot - Captures Settings") {

    val command = new SnapshotCommand {
      var capturedIndex = "not yet set"
      var capturedAgents = collection.mutable.Set[String]()
      override def bls(index: String): BLs = new BLs(index) {
        capturedIndex = index
        override def text: Stream[String] = Stream("<ag1>", "<ag2>")
      }
      override def bcat(agent: String): BCat = new BCat(agent) {
        capturedAgents = capturedAgents + agent
        val ag1Text = """. {cname:ag1,slave:}
          |1.254 n""".stripMargin
        val ag2Text = """. {cname:ag2,slave:}
          |1 {comment:next_line_is_empty}
          |1.254
          |2 {comment:next_line_ends_with_one_space}
          |2.254 
          |2.2.254 two words""".stripMargin
        override def text: Stream[String] =
          (if (agent == "<main:ag1>") ag1Text else ag2Text).lines.toStream
      }
    }

    val catcher = new PrintCatcher

    val setup = command.action(List())(Setup(), catcher.println)

    val expectedSettings = Map(
      "<main:ag1>#1" -> "n",
      "<main:ag2>#2" -> "",
      "<main:ag2>#2.2" -> "two words")

    setup.allSettings should equal(expectedSettings)
  }

  // -------------------------------------------------------------------------------
  //
  // Up
  //
  // -------------------------------------------------------------------------------

  test("Up - Can go up a level") {
    val connsTop = Connection("<rig1>#1.1", "<top>#20.2")
    val connsBot = Connection("<main.rig1:rig2>#2.3", "<main.rig1:mid>#14.3")

    val setup1 = Setup(Set(connsTop, connsBot)).withPosUpdated(List("<rig1>"))

    val command = new UpCommand
    val catcher = new PrintCatcher
    val setup2 = command.action(List())(setup1, catcher.println)

    setup2.pos should equal(List())
    catcher.output should include("Position: Top level")
  }

  test("Up - Can't go beyond top level") {
    val connsTop = Connection("<rig1>#1.1", "<top>#20.2")
    val connsBot = Connection("<main.rig1:rig2>#2.3", "<main.rig1:mid>#14.3")

    val setup1 = Setup(Set(connsTop, connsBot)).withPosUpdated(List())

    val command = new UpCommand
    val catcher = new PrintCatcher
    val setup2 = command.action(List())(setup1, catcher.println)

    setup2.pos should equal(List())
    catcher.output should include("Already at top level")
    catcher.output should include("Position: Top level")
  }

  test("Up - Rejects too many arguments") {
    val connsTop = Connection("<rig1>#1.1", "<top>#20.2")
    val connsBot = Connection("<main.rig1:rig2>#2.3", "<main.rig1:mid>#14.3")

    val setup1 = Setup(Set(connsTop, connsBot)).withPosUpdated(List("<rig1>"))

    val command = new UpCommand
    val catcher = new PrintCatcher
    val setup2 = command.action(List("something"))(setup1, catcher.println)

    setup2.pos should equal(List("<rig1>"))
    catcher.output should include("up: Does not take arguments")
    catcher.output should include("Position: <rig1>")
  }

}