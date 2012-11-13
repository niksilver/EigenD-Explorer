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
      assert(parsePhrase(outputLine, "1 hello") === Some(StateVariableLine("1", "hello")))
      assert(parsePhrase(outputLine, "12 hello") === Some(StateVariableLine("12", "hello")))
      assert(parsePhrase(outputLine, "12.34 hello") === Some(StateVariableLine("12.34", "hello")))
      assert(parsePhrase(outputLine, "12.34.56 hello") === Some(StateVariableLine("12.34.56", "hello")))
      assert(parsePhrase(outputLine, "something else") === None)
    }
  }

  test("Key-value pairs (single value)") {
    new BCatOutputParser {
      assert(parsePhrase(keyValuePair, "key1:value1") ===
        Some(("key1" -> List("value1"))))
      assert(parsePhrase(keyValuePair, "key1: value1 ") ===
        Some(("key1" -> List(" value1 "))))
    }
  }
  
  ignore("Read dictionary string (single values)") {
    new BCatOutputParser {
      assert(parsePhrase(dictionary, "{}") === Some(Map()))
      assert(parsePhrase(dictionary, "{ }") === Some(Map()))
      assert(parsePhrase(dictionary, "{ key1:value1 }") === Some(Map("key1" -> List("value1"))))
      assert(parsePhrase(dictionary, "{ key1:value1, key2 : value2 }") ===
        Some(Map("key1" -> List("value1"), "key2" -> List("value2"))))
      assert(parsePhrase(dictionary, "{ aa: bb, cc: dd, ee: ff }") ===
        Some(Map("aa" -> List("bb"), "cc" -> List("dd"), "ee" -> List("ff"))))

      assert(parsePhrase(dictionary, "something else") === None)
    }
  }

  ignore("Read dictionary string (multi-values)") {
    new BCatOutputParser {
      assert(parsePhrase(dictionary, "{ key1:value1a value2a }") === Some(Map("key1" -> List("value1a", "value2a"))))
    }
  }

  ignore("Read dictionary string (no values)") {
    new BCatOutputParser {
      assert(parsePhrase(dictionary, "{ key1: }") === Some(Map("key1" -> List())))
    }
  }

  ignore("Unified value") {
    new BCatOutputParser {
      assert(parsePhrase(bareValue, "hello") === Some("hello"))
      assert(parsePhrase(bareValue, "h-ello") === Some("h-ello"))
      assert(parsePhrase(bareValue, "he(l)lo") === None)
      assert(parsePhrase(bareValue, "he[llo") === None)
      assert(parsePhrase(bareValue, "he]llo") === None)
      assert(parsePhrase(bareValue, "he}llo") === None)
      assert(parsePhrase(bareValue, "he{llo") === None)
      assert(parsePhrase(bareValue, "he<llo") === None)
      assert(parsePhrase(bareValue, "he>llo") === None)
      assert(parsePhrase(bareValue, "he'llo") === None)
      assert(parsePhrase(bareValue, "he llo") === None)
      assert(parsePhrase(bareValue, "he,llo") === None)
      assert(parsePhrase(bareValue, "he:llo") === None)
    }
  }
  
  test("Bare value") {
    new BCatOutputParser {
      assert(parsePhrase(value, "hello") === Some("hello"))

      assert(parsePhrase(value, " value1") === Some(" value1"))
      assert(parsePhrase(value, " value1 ") === Some(" value1 "))
    }
  }

  ignore("Value") {
    new BCatOutputParser {
      assert(parsePhrase(value, "hello") === Some("hello"))

      assert(parsePhrase(value, " value1") === Some(" value1"))
      assert(parsePhrase(value, " value1 ") === Some(" value1 "))

      assert(parsePhrase(value, "(hello)") === Some("(hello)"))
      assert(parsePhrase(value, "h(ello)") === Some("h(ello)"))
      assert(parsePhrase(value, "h(ell)o") === Some("h(ell)o"))
      assert(parsePhrase(value, "h(e ll)o") === Some("h(e ll)o"))
      assert(parsePhrase(value, "h(e, ll)o") === Some("h(e, ll)o"))
      assert(parsePhrase(value, "h(e:ll)o") === Some("h(e:ll)o"))
      assert(parsePhrase(value, "h()ello") === Some("h()ello"))

      assert(parsePhrase(value, "(h(el)lo)") === Some("(h(el)lo)"))
      assert(parsePhrase(value, "h(el()lo)") === Some("h(el()lo)"))
      assert(parsePhrase(value, "h((e ll))o") === Some("h((e ll))o"))
      assert(parsePhrase(value, "h((e, ll))o") === Some("h((e, ll))o"))
      assert(parsePhrase(value, "h((e:ll))o") === Some("h((e:ll))o"))

      assert(parsePhrase(value, "<hello>") === Some("<hello>"))
      assert(parsePhrase(value, "h<ello>") === Some("h<ello>"))
      assert(parsePhrase(value, "h<ell>o") === Some("h<ell>o"))
      assert(parsePhrase(value, "h<e ll>o") === Some("h<e ll>o"))
      assert(parsePhrase(value, "h<e, ll>o") === Some("h<e, ll>o"))
      assert(parsePhrase(value, "h<e:ll>o") === Some("h<e:ll>o"))
      assert(parsePhrase(value, "h<>ello") === Some("h<>ello"))

      assert(parsePhrase(value, "<h(el)lo>") === Some("<h(el)lo>"))
      assert(parsePhrase(value, "h(el<>lo)") === Some("h(el<>lo)"))
      assert(parsePhrase(value, "h(<e ll>)o") === Some("h(<e ll>)o"))
      assert(parsePhrase(value, "h((e, ll))o") === Some("h((e, ll))o"))
      assert(parsePhrase(value, "h<<e:ll>>o") === Some("h<<e:ll>>o"))

      assert(parsePhrase(value, "{h(el)lo}") === Some("{h(el)lo}"))
      assert(parsePhrase(value, "h(el{}lo)") === Some("h(el{}lo)"))
      assert(parsePhrase(value, "h({e ll})o") === Some("h({e ll})o"))
      assert(parsePhrase(value, "h((e, ll))o") === Some("h((e, ll))o"))
      assert(parsePhrase(value, "h{{e:ll}}o") === Some("h{{e:ll}}o"))

      assert(parsePhrase(value, "[h(el)lo]") === Some("[h(el)lo]"))
      assert(parsePhrase(value, "h(el[]lo)") === Some("h(el[]lo)"))
      assert(parsePhrase(value, "h([e ll])o") === Some("h([e ll])o"))
      assert(parsePhrase(value, "h((e, ll))o") === Some("h((e, ll))o"))
      assert(parsePhrase(value, "h[[e:ll]]o") === Some("h[[e:ll]]o"))

      assert(parsePhrase(value, "'hello'") === Some("'hello'"))
      assert(parsePhrase(value, "'hell,o'") === Some("'hell,o'"))
      assert(parsePhrase(value, "'hell)o'") === Some("'hell)o'"))

      assert(parsePhrase(value, "[h'el'lo]") === Some("[h'el'lo]"))
      assert(parsePhrase(value, "h(el''lo)") === Some("h(el''lo)"))
      assert(parsePhrase(value, "h'e ll'o") === Some("h'e ll'o"))
      assert(parsePhrase(value, "h('e, ll')o") === Some("h('e, ll')o"))
      assert(parsePhrase(value, "h'[e:ll'o") === Some("h'[e:ll'o"))

      assert(parsePhrase(value, "h (ello)") === None)
      assert(parsePhrase(value, "hel)lo") === None)
      assert(parsePhrase(value, "hel(lo") === None)
      assert(parsePhrase(value, "hello,") === None)
    }

  }

}
