package org.pigsaw.eigendmapper

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.pigsaw.eigendmapper._
import org.pigsaw.eigendmapper.Mapper._

@RunWith(classOf[JUnitRunner])
class DictionarySuite extends FunSuite {

  test("Read dictionary line") {
    new BCatOutputParser {
      assert(parseWhole(outputLine, "1 hello") === Some(StateVariableLine("1", "hello")))
      assert(parseWhole(outputLine, "12 hello") === Some(StateVariableLine("12", "hello")))
      assert(parseWhole(outputLine, "12.34 hello") === Some(StateVariableLine("12.34", "hello")))
      assert(parseWhole(outputLine, "12.34.56 hello") === Some(StateVariableLine("12.34.56", "hello")))
      assert(parseWhole(outputLine, "something else") === None)
    }
  }

  test("Read dictionary string (single values)") {
    new BCatOutputParser {
      assert(parseWhole(dictionary, "{}") === Some(Map()))
      assert(parseWhole(dictionary, "{ }") === Some(Map()))
      assert(parseWhole(dictionary, "{ key1:value1 }") === Some(Map("key1" -> List("value1"))))
      assert(parseWhole(dictionary, "{ key1:value1, key2 : value2 }") ===
        Some(Map("key1" -> List("value1"), "key2" -> List("value2"))))
      assert(parseWhole(dictionary, "{ aa: bb, cc: dd, ee: ff }") ===
        Some(Map("aa" -> List("bb"), "cc" -> List("dd"), "ee" -> List("ff"))))

      assert(parseWhole(dictionary, "something else") === None)
    }
  }

  test("Read dictionary string (multi-values)") {
    new BCatOutputParser {
      assert(parseWhole(dictionary, "{ key1:value1a value2a }") === Some(Map("key1" -> List("value1a", "value2a"))))
    }
  }

  test("Read dictionary string (no values)") {
    new BCatOutputParser {
      assert(parseWhole(dictionary, "{ key1: }") === Some(Map("key1" -> List())))
    }
  }

  test("Unified value") {
    new BCatOutputParser {
      assert(parseWhole(bareValue, "hello") === Some("hello"))
      assert(parseWhole(bareValue, "h-ello") === Some("h-ello"))
      assert(parseWhole(bareValue, "he(l)lo") === None)
      assert(parseWhole(bareValue, "he[llo") === None)
      assert(parseWhole(bareValue, "he]llo") === None)
      assert(parseWhole(bareValue, "he}llo") === None)
      assert(parseWhole(bareValue, "he{llo") === None)
      assert(parseWhole(bareValue, "he<llo") === None)
      assert(parseWhole(bareValue, "he>llo") === None)
      assert(parseWhole(bareValue, "he'llo") === None)
      assert(parseWhole(bareValue, "he llo") === None)
      assert(parseWhole(bareValue, "he,llo") === None)
      assert(parseWhole(bareValue, "he:llo") === None)
    }
  }

  test("Value") {
    new BCatOutputParser {
      assert(parseWhole(value, "hello") === Some("hello"))

      assert(parseWhole(value, "(hello)") === Some("(hello)"))
      assert(parseWhole(value, "h(ello)") === Some("h(ello)"))
      assert(parseWhole(value, "h(ell)o") === Some("h(ell)o"))
      assert(parseWhole(value, "h(e ll)o") === Some("h(e ll)o"))
      assert(parseWhole(value, "h(e, ll)o") === Some("h(e, ll)o"))
      assert(parseWhole(value, "h(e:ll)o") === Some("h(e:ll)o"))
      assert(parseWhole(value, "h()ello") === Some("h()ello"))

      assert(parseWhole(value, "(h(el)lo)") === Some("(h(el)lo)"))
      assert(parseWhole(value, "h(el()lo)") === Some("h(el()lo)"))
      assert(parseWhole(value, "h((e ll))o") === Some("h((e ll))o"))
      assert(parseWhole(value, "h((e, ll))o") === Some("h((e, ll))o"))
      assert(parseWhole(value, "h((e:ll))o") === Some("h((e:ll))o"))

      assert(parseWhole(value, "<hello>") === Some("<hello>"))
      assert(parseWhole(value, "h<ello>") === Some("h<ello>"))
      assert(parseWhole(value, "h<ell>o") === Some("h<ell>o"))
      assert(parseWhole(value, "h<e ll>o") === Some("h<e ll>o"))
      assert(parseWhole(value, "h<e, ll>o") === Some("h<e, ll>o"))
      assert(parseWhole(value, "h<e:ll>o") === Some("h<e:ll>o"))
      assert(parseWhole(value, "h<>ello") === Some("h<>ello"))

      assert(parseWhole(value, "<h(el)lo>") === Some("<h(el)lo>"))
      assert(parseWhole(value, "h(el<>lo)") === Some("h(el<>lo)"))
      assert(parseWhole(value, "h(<e ll>)o") === Some("h(<e ll>)o"))
      assert(parseWhole(value, "h((e, ll))o") === Some("h((e, ll))o"))
      assert(parseWhole(value, "h<<e:ll>>o") === Some("h<<e:ll>>o"))

      assert(parseWhole(value, "{h(el)lo}") === Some("{h(el)lo}"))
      assert(parseWhole(value, "h(el{}lo)") === Some("h(el{}lo)"))
      assert(parseWhole(value, "h({e ll})o") === Some("h({e ll})o"))
      assert(parseWhole(value, "h((e, ll))o") === Some("h((e, ll))o"))
      assert(parseWhole(value, "h{{e:ll}}o") === Some("h{{e:ll}}o"))

      assert(parseWhole(value, "[h(el)lo]") === Some("[h(el)lo]"))
      assert(parseWhole(value, "h(el[]lo)") === Some("h(el[]lo)"))
      assert(parseWhole(value, "h([e ll])o") === Some("h([e ll])o"))
      assert(parseWhole(value, "h((e, ll))o") === Some("h((e, ll))o"))
      assert(parseWhole(value, "h[[e:ll]]o") === Some("h[[e:ll]]o"))

      assert(parseWhole(value, "'hello'") === Some("'hello'"))
      assert(parseWhole(value, "'hell,o'") === Some("'hell,o'"))
      assert(parseWhole(value, "'hell)o'") === Some("'hell)o'"))

      assert(parseWhole(value, "[h'el'lo]") === Some("[h'el'lo]"))
      assert(parseWhole(value, "h(el''lo)") === Some("h(el''lo)"))
      assert(parseWhole(value, "h'e ll'o") === Some("h'e ll'o"))
      assert(parseWhole(value, "h('e, ll')o") === Some("h('e, ll')o"))
      assert(parseWhole(value, "h'[e:ll'o") === Some("h'[e:ll'o"))

      assert(parseWhole(value, "h (ello)") === None)
      assert(parseWhole(value, "hel)lo") === None)
      assert(parseWhole(value, "hel(lo") === None)
      assert(parseWhole(value, "hello,") === None)
    }

  }

}
