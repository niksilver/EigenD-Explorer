package org.pigsaw.eigendmapper

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers

@RunWith(classOf[JUnitRunner])
class BCatSuite extends FunSuite with ShouldMatchers {

  trait TestParser extends BCatParser {
    def parseOption[T](parser: Parser[T], dictstr: String): Option[T] =
      parseAll(parser, dictstr) match {
        case Success(out, _) => Some(out)
        case fail => None
      }
  }

  test("Read bcat lines") {
    val output = """"log:using portbase 55555
      |. {cname:metronome,cordinal:1}
      |1 {cname:outputs,protocols:}
      |1.1 one point one
      |1.3.254""".stripMargin

    val bcat = new BCat("<something>") {
      override def text: Stream[String] = output.lines.toStream
    }

    assert(bcat.state === Map(
      "." -> DictValue(Map("cname" -> List("metronome"), "cordinal" -> List("1"))),
      "1" -> DictValue(Map("cname" -> List("outputs"), "protocols" -> List())),
      "1.1" -> StringValue("one point one")))
  }

  test("Connections - slaves") {
    val output = """"log:using portbase 5555
      |. {cname:clicker,slave:}
      |1.2 {cname:oner,protocols:output,slave:'<drummer1>#2.1'}
      |3.5 {cname:twoer,slave:'<rig2>#4','<clarinet7>#8.8.8',protocols:output}
      |1.3.254""".stripMargin

    val bcat = new BCat("<clicker1>") {
      override def text: Stream[String] = output.lines.toStream
    }

    val connections = bcat.connections

    val master_port_1_2 = "<clicker1>#1.2"
    val slave_port_1_2 = "<drummer1>#2.1"

    val master_port_3_5 = "<clicker1>#3.5"
    val slave_port_3_5_a = "<rig2>#4"
    val slave_port_3_5_b = "<clarinet7>#8.8.8"
    assert(connections.size === 3)
    assert(connections contains Connection(master_port_1_2, slave_port_1_2))
    assert(connections contains Connection(master_port_3_5, slave_port_3_5_a))
    assert(connections contains Connection(master_port_3_5, slave_port_3_5_b))
  }

  test("Connections - masters") {
    val output = """"log:using portbase 5555
      |. {cname:metronome}
      |2 {domain:bfloat(),cname:tempo input,slave:,master:conn(None,None,'<controller1>#4.1.4',None,ctl),conn(None,None,'<interpreter1>#15.3',None,ctl)}
      |3.4.2 {domain:bfloat(),cname:click input}
      |4.8 {domain:bfloat(),cname:beat input,master:conn(None,None,'<interpreter1>#15.253.2',None,ctl)}
      |1.3.254""".stripMargin

    val bcat = new BCat("<metronome1>") {
      override def text: Stream[String] = output.lines.toStream
    }

    val connections = bcat.connections

    val master_port_2_a = "<controller1>#4.1.4"
    val master_port_2_b = "<interpreter1>#15.3"
    val slave_port_2 = "<metronome1>#2"

    val master_port_4_8 = "<interpreter1>#15.253.2"
    val slave_port_4_8 = "<metronome1>#4.8"

    assert(connections.size === 3)
    assert(connections contains Connection(master_port_2_a, slave_port_2))
    assert(connections contains Connection(master_port_2_b, slave_port_2))
    assert(connections contains Connection(master_port_4_8, slave_port_4_8))
  }

  test("Connections - masters and slaves together") {
    val output = """"log:using portbase 5555
      |. {cname:metronome}
      |2 {domain:bfloat(),cname:tempo input,slave:,master:conn(None,None,'<controller1>#4.1.4',None,ctl),conn(None,None,'<interpreter1>#15.3',None,ctl)}
      |3.4.2 {domain:bfloat(),cname:click input}
      |4.8 {domain:bfloat(),cname:beat input,master:conn(None,None,'<interpreter1>#15.253.2',None,ctl),slave:'<drummer1>#2.1'}
      |1.3.254""".stripMargin

    val bcat = new BCat("<metronome1>") {
      override def text: Stream[String] = output.lines.toStream
    }

    val connections = bcat.connections

    val master_port_2_a = "<controller1>#4.1.4"
    val master_port_2_b = "<interpreter1>#15.3"
    val slave_port_2 = "<metronome1>#2"

    val master_port_int_4_8 = "<interpreter1>#15.253.2"
    val slave_port_int_4_8 = "<metronome1>#4.8"

    val master_port_drum_4_8 = "<metronome1>#4.8"
    val slave_port_drum_4_8 = "<drummer1>#2.1"

    assert(connections.size === 4)
    assert(connections contains Connection(master_port_2_a, slave_port_2))
    assert(connections contains Connection(master_port_2_b, slave_port_2))
    assert(connections contains Connection(master_port_int_4_8, slave_port_int_4_8))
    assert(connections contains Connection(master_port_drum_4_8, slave_port_drum_4_8))
  }

  test("unqualifiedNodeIDNames - Reads cnames") {
    val output = """3.1 {protocols:input remove,ordinal:0,cname:bar beat}""".
      stripMargin

    val bcat = new BCat("<rig3>") {
      override def text: Stream[String] = output.lines.toStream
    }

    bcat.unqualifiedNodeIDNames should contain(("3.1" -> "bar beat"))
  }

  test("unqualifiedNodeIDNames - Reads names") {
    val output = """3.1 {protocols:input remove,name:bar beat}""".
      stripMargin

    val bcat = new BCat("<rig3>") {
      override def text: Stream[String] = output.lines.toStream
    }

    bcat.unqualifiedNodeIDNames should contain(("3.1" -> "bar beat"))
  }

  test("unqualifiedNodeIDNames - Names should trump cnames") {
    val output = """3.1 {name:bar beat,cname:bor boot}""".
      stripMargin

    val bcat = new BCat("<rig3>") {
      override def text: Stream[String] = output.lines.toStream
    }

    bcat.unqualifiedNodeIDNames should contain(("3.1" -> "bar beat"))
  }

  test("unqualifiedNodeIDNames - Includes cordinal with cname") {
    val output = """3.1 {cordinal:1,protocols:input remove,ordinal:0,cname:bar beat}""".
      stripMargin

    val bcat = new BCat("<rig3>") {
      override def text: Stream[String] = output.lines.toStream
    }

    bcat.unqualifiedNodeIDNames should contain(("3.1" -> "bar beat 1"))
  }

  test("unqualifiedNodeIDNames - Includes ordinal with name, if any") {
    val output = """3.1 {cordinal:1,protocols:input remove,ordinal:0,name:bar beat}
      |3.2 {name:bor boot}""".
      stripMargin

    val bcat = new BCat("<rig3>") {
      override def text: Stream[String] = output.lines.toStream
    }

    bcat.unqualifiedNodeIDNames should contain(("3.1" -> "bar beat 0"))
    bcat.unqualifiedNodeIDNames should contain(("3.2" -> "bor boot"))
  }

  test("unqualifiedNodeIDNames - Includes cordinal with name, if available and no ordinal") {
    val output = """3.1 {cordinal:1,protocols:input remove,name:bar beat}""".
      stripMargin

    val bcat = new BCat("<rig3>") {
      override def text: Stream[String] = output.lines.toStream
    }

    bcat.unqualifiedNodeIDNames should contain(("3.1" -> "bar beat 1"))
  }

  test("unqualifiedNodeIDNames - Trims cnames, names and cordinals") {
    val output = """2 {cname:audio outputs,protocols:}
    		|2.1 {cname:channels,protocols:}
    		|2.1.1 {cname:  audio output  ,cordinal:  1  }
    		|2.1.2 {name: audio output }
    		|"""".stripMargin

    val bcat = new BCat("<audio_unit1>") {
      override def text: Stream[String] = output.lines.toStream
    }

    bcat.unqualifiedNodeIDNames should contain(("2.1.1" -> "audio output 1"))
    bcat.unqualifiedNodeIDNames should contain(("2.1.2" -> "audio output"))
  }

  test("qualifiedNodeIDName - basic test") {
    val output = """3.1 {cordinal:1,cname:one}
      |3.1.2 {cname:a and b}
      |3.1.2.3 {cname:b or c,cordinal:1}
      |3.1.2.3.4 {cname:de or fg}
      |3.1.2.3.4.1 {cname:hi and jk,cordinal:1}
      |3.1.2.5 {cname:b or c,cordinal:2}
      |3.1.2.5.4 {cname:de or fg}
      |3.1.2.5.4.1 {cname:hi and jk,cordinal:1}
      """.stripMargin

    val bcat = new BCat("<rig3>") {
      override def text: Stream[String] = output.lines.toStream
    }

    bcat.qualifiedNodeIDName("3.1.2.3.4.1", 0) should equal("")
    bcat.qualifiedNodeIDName("3.1.2.3.4.1", 1) should equal("hi and jk 1")
    bcat.qualifiedNodeIDName("3.1.2.3.4.1", 2) should equal("de or fg hi and jk 1")
    bcat.qualifiedNodeIDName("3.1.2.3.4.1", 3) should equal("b or c 1 de or fg hi and jk 1")
  }

  test("qualifiedNodeIDName - Skips nodes which don't have names") {
    val output = """3.1 {cordinal:1,cname:one}
      |3.1.2.3 {cname:b or c,cordinal:1}
      |3.1.2.3.4.1 {cname:hi and jk,cordinal:1}
      |3.1.2.5 {cname:b or c,cordinal:2}
      |3.1.2.5.4 {cname:de or fg}
      |3.1.2.5.4.1 {cname:hi and jk,cordinal:1}
      """.stripMargin

    val bcat = new BCat("<rig3>") {
      override def text: Stream[String] = output.lines.toStream
    }

    bcat.qualifiedNodeIDName("3.1.2.3.4.1", 0) should equal("")
    bcat.qualifiedNodeIDName("3.1.2.3.4.1", 1) should equal("hi and jk 1")
    bcat.qualifiedNodeIDName("3.1.2.3.4.1", 2) should equal("hi and jk 1")
    bcat.qualifiedNodeIDName("3.1.2.3.4.1", 3) should equal("b or c 1 hi and jk 1")
    bcat.qualifiedNodeIDName("3.1.2.3.4.1", 4) should equal("b or c 1 hi and jk 1")
    bcat.qualifiedNodeIDName("3.1.2.3.4.1", 5) should equal("one 1 b or c 1 hi and jk 1")
  }

  test("nodeIDNames - Picks simplest unique qualified name") {
    // We have:
    //   3.1.2 - A deep name which is unique
    //   3.1.2.3.4.1 - A deep name which is unique only by going two steps up
    //   3.1.2.5.4.1 - The same deep name which is unique only by going two steps up

    val output = """3.1 {cordinal:1,cname:one}
      |3.1.2 {cname:a and b}
      |3.1.2.3 {cname:b or c,cordinal:1}
      |3.1.2.3.4 {cname:de or fg}
      |3.1.2.3.4.1 {cname:hi and jk,cordinal:1}
      |3.1.2.5 {cname:b or c,cordinal:2}
      |3.1.2.5.4 {cname:de or fg}
      |3.1.2.5.4.1 {cname:hi and jk,cordinal:1}
      """.stripMargin

    val bcat = new BCat("<rig3>") {
      override def text: Stream[String] = output.lines.toStream
    }

    bcat.nodeIDNames should contain(("3.1.2" -> "a and b"))
    bcat.nodeIDNames should contain(("3.1.2.3.4.1" -> "b or c 1 de or fg hi and jk 1"))
    bcat.nodeIDNames should contain(("3.1.2.5.4.1" -> "b or c 2 de or fg hi and jk 1"))
  }

  test("nodeIDNames - Handles non-unique (c)names in dict") {
    // We have:
    //   3.1.2 - A deep name which is unique
    //   3.1.2.3.4.1 - A deep name which is unique only by going two steps up
    //   3.1.2.5.4.1 - The same deep name which is unique only by going two steps up

    val output = """3.1 {cordinal:1,cname:one}
      |3.1.2 {cname:aa}
      |3.1.2.3 {cname:bb,cordinal:1}
      |3.1.2.5 {cname:bb,cordinal:1}
      """.stripMargin

    val bcat = new BCat("<rig3>") {
      override def text: Stream[String] = output.lines.toStream
    }

    bcat.nodeIDNames should contain(("3.1.2.3" -> "#3.1.2.3 one 1 aa bb 1"))
    bcat.nodeIDNames should contain(("3.1.2.5" -> "#3.1.2.5 one 1 aa bb 1"))
  }

  /*test("Real bcat output") {
    val bcat = new BCat("<metronome1>")
    bcat.state.toList map println
  }*/

  test("Settings - Will recognise unnamed settings") {
    val output = """"log:using portbase 5555
      |. {cname:metronome}
      |2 {domain:bfloat(),cname:tempo input}
      |3.3.254 0.0
      |3.4.2 {domain:bfloat(),cname:click input}
      |4.254 some value with spaces
      |4.8 {domain:bfloat(),cname:beat input}
      |5.6.7.254 y
      |1.3.254""".stripMargin

    val bcat = new BCat("<metronome1>") {
      override def text: Stream[String] = output.lines.toStream
    }

    val settings = bcat.settings

    settings.size should equal(3)
    settings should contain(("<metronome1>#3.3" -> "0.0"))
    settings should contain(("<metronome1>#4" -> "some value with spaces"))
    settings should contain(("<metronome1>#5.6.7" -> "y"))
  }

  test("Settings - Names node IDs correctly even if in a rig") {
    val output = """"log:using portbase 5555
      |. {cname:metronome}
      |3.3.254 0.0""".stripMargin

    val bcat = new BCat("<main.rig3:metronome1>") {
      override def text: Stream[String] = output.lines.toStream
    }

    val settings = bcat.settings

    settings.size should equal(1)
    settings should contain(("<metronome1>#3.3" -> "0.0"))
  }

  test("Settings - Will not use port names") {
    val output = """"log:using portbase 5555
      |. {cname:metronome}
      |3.3 {domain:bfloat(),cname:tempo input}
      |3.3.254 0.0
      |4 {domain:bstring(),cname:arbitrary thing}
      |4.254 some value with spaces
      |4.8 {domain:bfloat(),cname:beat input}
      |5.6.7 {domain:boolean(),cname:open}
      |5.6.7.254 y
      |1.3.254""".stripMargin

    val bcat = new BCat("<metronome1>") {
      override def text: Stream[String] = output.lines.toStream
    }

    val settings = bcat.settings

    settings.size should equal(3)
    settings should contain(("<metronome1>#3.3" -> "0.0"))
    settings should contain(("<metronome1>#4" -> "some value with spaces"))
    settings should contain(("<metronome1>#5.6.7" -> "y"))
  }

}
