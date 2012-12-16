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
    val output = """"log:using portbase 5555
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
    
    val master_port_1_2 = Port("<clicker1>#1.2", Some("oner"))
    val slave_port_1_2 = Port("<drummer1>#2.1", None)
    
    val master_port_3_5 = Port("<clicker1>#3.5", Some("twoer"))
    val slave_port_3_5_a = Port("<rig2>#4", None)
    val slave_port_3_5_b = Port("<clarinet7>#8.8.8", None)
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
    
    val master_port_2_a = Port("<controller1>#4.1.4", None)
    val master_port_2_b = Port("<interpreter1>#15.3", None)
    val slave_port_2 = Port("<metronome1>#2", Some("tempo input"))
    
    val master_port_4_8 = Port("<interpreter1>#15.253.2", None)
    val slave_port_4_8 = Port("<metronome1>#4.8", Some("beat input"))

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
    
    val master_port_2_a = Port("<controller1>#4.1.4", None)
    val master_port_2_b = Port("<interpreter1>#15.3", None)
    val slave_port_2 = Port("<metronome1>#2", Some("tempo input"))
    
    val master_port_int_4_8 = Port("<interpreter1>#15.253.2", None)
    val slave_port_int_4_8 = Port("<metronome1>#4.8", Some("beat input"))
    
    val master_port_drum_4_8 = Port("<metronome1>#4.8", Some("beat input"))
    val slave_port_drum_4_8 = Port("<drummer1>#2.1", None)

    assert(connections.size === 4)
    assert(connections contains Connection(master_port_2_a, slave_port_2))
    assert(connections contains Connection(master_port_2_b, slave_port_2))
    assert(connections contains Connection(master_port_int_4_8, slave_port_int_4_8))
    assert(connections contains Connection(master_port_drum_4_8, slave_port_drum_4_8))
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
    
    settings.size should equal (3)
    settings should contain (("<metronome1>#3.3" -> "0.0"))
    settings should contain (("<metronome1>#4" -> "some value with spaces"))
    settings should contain (("<metronome1>#5.6.7" -> "y"))
  }
  
  test("Settings - Names node IDs correctly even if in a rig") {
    val output = """"log:using portbase 5555
      |. {cname:metronome}
      |3.3.254 0.0""".stripMargin

    val bcat = new BCat("<main.rig3:metronome1>") {
      override def text: Stream[String] = output.lines.toStream
    }
    
    val settings = bcat.settings
    
    settings.size should equal (1)
    settings should contain (("<metronome1>#3.3" -> "0.0"))
  }

}