package org.pigsaw.eigendmapper

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.pigsaw.eigendmapper._
import org.pigsaw.eigendmapper.Mapper._

@RunWith(classOf[JUnitRunner])
class BCatMapperSuite extends FunSuite {

  trait TestParser extends BCatParser {
    def parseOption[T](parser: Parser[T], dictstr: String): Option[T] =
      parseAll(parser, dictstr) match {
        case Success(out, _) => Some(out)
        case fail => None
      }
  }

  test("Read bcat line - text") {
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

  /*test("Real bcat output") {
    val bcat = new BCat("<metronome1>")
    bcat.state.toList map println
  }*/

}
