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
    catcher.output should include("No agent called <rig1>")
  }

  test("Show - Produces somewhat sensible output") {
    val conn = Connection("<ttt> three", "<fff> five")
    val setup = Setup(Set(conn))

    val catcher = new PrintCatcher

    (new ShowCommand).action(List("<ttt>"))(setup, catcher.println)

    catcher.output should not include ("Unknown")
    catcher.output should include("-->")
  }

  test("Show - Handles being in a rig") {
    val connTop = Connection("<rig1> one", "<fff> five")
    val connRig = Connection("<aaa> ayes", "<bbb> bees")
    
    val setupRig = new Setup(Map(), Set(connRig), Map(), List())
    val setupTop = new Setup(Map(), Set(connTop), Map("<rig1>" -> setupRig), List("<rig1>"))

    val catcher = new PrintCatcher

    (new ShowCommand).action(List("<aaa>"))(setupTop, catcher.println)

    catcher.output should not include ("Unknown")
    catcher.output should not include ("No agent called")
    catcher.output should include("ayes --> <bbb> bees")

  }

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

    val connsRig = Connection("<too> two out", "<mid> mid input")
    val setupRig = new Setup(Map(), Set(connsRig), Map(), List())

    val connsTop = Connection("<rig1> three", "<fff> five")

    val catcher = new PrintCatcher

    val setup = new Setup(Map(), Set(connsTop), Map("<rig1>" -> setupRig), List("<rig1>"))
    val setupV2 = command.action(List())(setup, catcher.println)

    command.capturedIndex should equal("<main.rig1:main>")
    command.capturedAgents should equal(Set("<main.rig1:too>", "<main.rig1:mid>"))
  }

  test("Snapshot - Preserves other setup data") {
    val connsTop = Connection("<rig1> one out", "<top> top input")
    val connsRig = Connection("<too> two out", "<mid> mid input")

    val setupRig = new Setup(Map(), Set(connsRig), Map(), List())
    val setupTop = new Setup(Map(), Set(connsTop), Map("<rig1>" -> setupRig), List("<rig1>"))

    val connsRigV2 = Connection("<too> two out2", "<mid> mid input2")

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

    setupTop2.conns should equal(Set(connsTop))
    setupTop2.rigSetups.keySet should equal(Set("<rig1>"))
    setupTop2.pos should equal(List("<rig1>"))

    setupTop2.rigSetups("<rig1>").conns should equal(Set(connsRigV2))
    setupTop2.rigSetups("<rig1>").rigSetups.keys should equal(Set())
    setupTop2.rigSetups("<rig1>").pos should equal(List())
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
          (if (agent == "<ag1>") ag1Text else ag2Text).lines.toStream
      }
    }

    val catcher = new PrintCatcher

    val setup = command.action(List())(Setup(), catcher.println)
    
    setup.conns.size should equal (5)
    
    setup.ports should contain ("<ag1> one one")
    setup.ports should contain ("<ag1>#1.2")
    setup.ports should contain ("<ag1>#1.3")
    setup.ports should contain ("<ag1>#1.4")
    setup.ports should contain ("<ag1>#1.22")
    
    setup.ports should contain ("<ag2>#2.1")
    setup.ports should contain ("<ag2> two two")
    setup.ports should contain ("<ag2> two three")
    setup.ports should contain ("<ag2>#2.4")
    
    setup.conns should contain (Connection("<ag1> one one", "<ag2>#2.1"))
    setup.conns should contain (Connection("<ag1>#1.2", "<ag2> two two"))
    setup.conns should contain (Connection("<ag1>#1.3", "<ag2> two three"))
    setup.conns should contain (Connection("<ag1>#1.4", "<ag2>#2.4"))
    setup.conns should contain (Connection("<ag2> two two", "<ag1>#1.22"))
  }

  test("Into - Can go into an empty rig") {
    val connsTop = Connection("<rig1> one out", "<top> top input")
    val connsMid = Connection("<rig2> two out", "<mid> mid input")
    val connsBottom = Connection("<rig3> three out", "<bottom> bottom input")

    val setupBottom = new Setup(Set(connsBottom))
    val setupMid = new Setup(Map(), Set(connsMid), Map("<rig2>" -> setupBottom), List())
    val setupTop = new Setup(Map(), Set(connsTop), Map("<rig1>" -> setupMid), List("<rig1>", "<rig2>"))

    val command = new IntoCommand
    val catcher = new PrintCatcher
    val setupTop2 = command.action(List("<rig3>"))(setupTop, catcher.println)

    setupTop2.pos should equal(List("<rig1>", "<rig2>", "<rig3>"))
    catcher.output should include("Position: <rig1> - <rig2> - <rig3>")
  }

  test("Into - Can go into an already-present rig") {
    val connsTop = Connection("<rig1> one out", "<top> top input")
    val connsMid = Connection("<rig2> two out", "<mid> mid input")
    val connsBottom = Connection("<free> three out", "<bottom> bottom input")

    val setupBottom = new Setup(Set(connsBottom))
    val setupMid = new Setup(Map(), Set(connsMid), Map("<rig2>" -> setupBottom), List())
    val setupTop = new Setup(Map(), Set(connsTop), Map("<rig1>" -> setupMid), List("<rig1>"))

    val command = new IntoCommand
    val catcher = new PrintCatcher
    val setupTop2 = command.action(List("<rig2>"))(setupTop, catcher.println)

    setupTop2.pos should equal(List("<rig1>", "<rig2>"))
    catcher.output should include("Position: <rig1> - <rig2>")
  }

  test("Into - Can't go into a non-existent rig") {
    val connsTop = Connection("<rig1> one out", "<top> top input")
    val connsMid = Connection("<rig2> two out", "<mid> mid input")
    val connsBottom = Connection("<free> three out", "<bottom> bottom input")

    val setupBottom = new Setup(Set(connsBottom))
    val setupMid = new Setup(Map(), Set(connsMid), Map("<rig2>" -> setupBottom), List())
    val setupTop = new Setup(Map(), Set(connsTop), Map("<rig1>" -> setupMid), List("<rig1>"))

    val command = new IntoCommand
    val catcher = new PrintCatcher
    val setupTop2 = command.action(List("<rig77>"))(setupTop, catcher.println)

    setupTop2.pos should equal(List("<rig1>"))
    catcher.output.lines.toList(0) should equal("No such rig: <rig77>")
    catcher.output.lines.toList(1) should equal("Position: <rig1>")
  }

  test("Into - Can't go into non-existent rig at top level ") {
    val connsTop = Connection("<rig1> one out", "<top> top input")
    val connsMid = Connection("<rig2> two out", "<mid> mid input")
    val connsBottom = Connection("<free> three out", "<bottom> bottom input")

    val setupBottom = new Setup(Set(connsBottom))
    val setupMid = new Setup(Map(), Set(connsMid), Map("<rig2>" -> setupBottom), List())
    val setupTop = new Setup(Map(), Set(connsTop), Map("<rig1>" -> setupMid), List())

    val command = new IntoCommand
    val catcher = new PrintCatcher
    val setupTop2 = command.action(List("<rig77>"))(setupTop, catcher.println)

    setupTop2.pos should equal(List())
    catcher.output.lines.toList(0) should equal("No such rig: <rig77>")
    catcher.output.lines.toList(1) should equal("Position: Top level")
  }

  test("Into - Handles bad arguments") {
    val connsTop = Connection("<rig1> one out", "<top> top input")
    val setupTop = new Setup(Map(), Set(connsTop), Map(), List())

    val command = new IntoCommand

    val catcher1 = new PrintCatcher
    command.action(List())(setupTop, catcher1.println)
    catcher1.output.lines.toList(0) should equal("into: Too few arguments")
    catcher1.output.lines.toList(1) should equal("Position: Top level")

    val catcher2 = new PrintCatcher
    command.action(List("a", "b"))(setupTop, catcher2.println)
    catcher2.output.lines.toList(0) should equal("into: Too many arguments")
    catcher2.output.lines.toList(1) should equal("Position: Top level")
  }

  ignore("Agents command") {}
}