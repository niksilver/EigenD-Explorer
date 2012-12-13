package org.pigsaw.eigendmapper

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

  test("Show - handles no agents") {
    val setup = Setup()
    
    val catcher = new PrintCatcher
    
    (new ShowCommand).action(List("<rig1>"))(setup, catcher.println)
    
    catcher.output should not include ("Unknown")
    catcher.output should include ("No agent called <rig1>")
  }

  test("Show - Produces somewhat sensible output") {
    val conn = Connection(Port("<ttt>#3.3", Some("three")), Port("<fff>#5.5", Some("five")))
    val setup = Setup(Set(conn))
    
    val catcher = new PrintCatcher
    
    (new ShowCommand).action(List("<ttt>"))(setup, catcher.println)
    
    catcher.output should not include ("Unknown")
    catcher.output should include ("-->")
  }

  test("Graph - handles no arguments") {
    val setup = Setup()
    
    val catcher = new PrintCatcher
    val args = List()
    
    (new GraphCommand).action(args)(setup, catcher.println)
    
    catcher.output should include ("You need to specify")
  }

  test("Graph - handles too many args") {
    val setup = Setup()
    
    val catcher = new PrintCatcher
    val args = List("a", "b")
    
    (new GraphCommand).action(args)(setup, catcher.println)
    
    catcher.output should include ("Too many arguments")
  }

  test("Graph - handles single bad argument") {
    val setup = Setup()
    
    val catcher = new PrintCatcher
    val args = List("wrong")
    
    (new GraphCommand).action(args)(setup, catcher.println)
    
    catcher.output should include ("Do not recognise what to graph")
  }
  
  test("SnapshotCommand.index") {
    val command = new SnapshotCommand
    
    command.index(List()) should equal ("<main>")
    command.index(List("<rig1>")) should equal ("<main.rig1:main>")
    command.index(List("<rig1>", "<rig2>")) should equal ("<main.rig1:main.rig2:main>")
  }
  
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
    
    command.capturedIndex should equal ("<main>")
  }
  
  test("Snapshot - Correct bls index for first level down") {
    
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

    val connsRig = Connection(Port("<too>#2.2", Some("two out")), Port("<mid>#7.7", Some("mid input")))
    val setupRig = new Setup(Set(connsRig), Map(), List())
    
    val catcher = new PrintCatcher
    
    val setup = new Setup(Set(), Map("<rig1>" -> setupRig), List("<rig1>"))
    val setupV2 = command.action(List())(setup, catcher.println)
    
    command.capturedIndex should equal ("<main.rig1:main>")
  }
  
  test("Snapshot - Preserves other setup data") {
    val connsTop = Connection(Port("<rig1>#1.1", Some("one out")), Port("<top>#5.5", Some("top input")))
    val connsRig = Connection(Port("<too>#2.2", Some("two out")), Port("<mid>#7.7", Some("mid input")))
    
    val setupRig = new Setup(Set(connsRig), Map(), List())
    val setupTop = new Setup(Set(connsTop), Map("<rig1>" -> setupRig), List("<rig1>"))

    val connsRigV2 = Connection(Port("<too>#22.22", Some("two out2")), Port("<mid>#72.72", Some("mid input2")))

    val command = new SnapshotCommand {
      override def bls(index: String): BLs = new BLs(index) {
        override def agents: List[String] = List("<rig1>")
      }
      override def bcat(agent: String): BCat = new BCat(agent) {
        override lazy val connections: Set[Connection] = Set(connsRigV2)
      }
    }
    
    val catcher = new PrintCatcher
    val setupTop2 = command.action(List())(setupTop, catcher.println)
    
    setupTop2.conns should equal (Set(connsTop))
    setupTop2.rigSetups.keySet should equal (Set("<rig1>"))
    setupTop2.pos should equal (List("<rig1>"))
    
    setupTop2.rigSetups("<rig1>").conns should equal (Set(connsRigV2))
    setupTop2.rigSetups("<rig1>").rigSetups.keys should equal (Set())
    setupTop2.rigSetups("<rig1>").pos should equal (List())
  }
}