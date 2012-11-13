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

  test("Some values") {
    new BCatOutputParser {
      assert(parsePhrase(someValues, "value1") ===
        Some(List("value1")))
      assert(parsePhrase(someValues, "value1a,value1b") ===
        Some(List("value1a", "value1b")))
    }
  }

  test("Key-value pairs (single value)") {
    new BCatOutputParser {
      assert(parsePhrase(keyValuePairs, "key1:value1") ===
        Some(Map("key1" -> List("value1"))))

      assert(parsePhrase(keyValuePairs, "key1: value1 ") ===
        Some(Map("key1" -> List(" value1 "))))

      assert(parsePhrase(keyValuePairs, "key1:value1,key2:value2") ===
        Some(Map("key1" -> List("value1"), "key2" -> List("value2"))))
    }
  }

  test("Key-value pairs (multi-values)") {
    new BCatOutputParser {
      assert(parsePhrase(keyValuePairs, "key1:value1a,value1b") ===
        Some(Map("key1" -> List("value1a", "value1b"))))
    }
  }
  
  test("keyValueStringsToMap") {
    new BCatOutputParser {
      assert(keyValueStringsToMap(List("key1", ":", "value1")) === Map("key1" -> List("value1")))
      
      assert(keyValueStringsToMap(
          List("key1", ":", "value1", ",", "key2", ":", "value2")
          ) === Map("key1" -> List("value1"), "key2" -> List("value2")))
          
      assert(keyValueStringsToMap(
          List("key1", ":", "value1a", ",", "value1b", ",", "key2", ":", "value2")
          ) === Map("key1" -> List("value1a", "value1b"), "key2" -> List("value2")))
          
      assert(keyValueStringsToMap(
          List("key1", ":", "value1", ",", "key2", ":", "value2a", ",", "value2b")
          ) === Map("key1" -> List("value1"), "key2" -> List("value2a", "value2b")))
          
      assert(keyValueStringsToMap(
          List("key1", ":", ",", "key2", ":", "value2")
          ) === Map("key1" -> List(), "key2" -> List("value2")))
          
      assert(keyValueStringsToMap(
          List("key1", ":", "value1", ",", "key2", ":")
          ) === Map("key1" -> List("value1"), "key2" -> List()))
    }
  }

  test("Key-value pairs") {
    new BCatOutputParser {
      assert(parsePhrase(keyValuePairs, "key1:value1") ===
        Some(Map("key1" -> List("value1"))))

      assert(parsePhrase(keyValuePairs, "key1:value1,key2:value2") ===
        Some(Map("key1" -> List("value1"), "key2" -> List("value2"))))
    }
  }

  test("Read dictionary string (single values)") {
    new BCatOutputParser {
      assert(parsePhrase(dictionary, "{}") === Some(Map()))

      assert(parsePhrase(dictionary, "{key1:value1}") === Some(Map("key1" -> List("value1"))))

      assert(parsePhrase(dictionary, "{key1: value1 }") === Some(Map("key1" -> List(" value1 "))))

      assert(parsePhrase(dictionary, "{key1:value1,key2:value2}") ===
        Some(Map("key1" -> List("value1"), "key2" -> List("value2"))))

      assert(parsePhrase(dictionary, "{key1:value1,key2: value2 }") ===
        Some(Map("key1" -> List("value1"), "key2" -> List(" value2 "))))

      assert(parsePhrase(dictionary, "{aa:bb,cc:dd,ee:ff}") ===
        Some(Map("aa" -> List("bb"), "cc" -> List("dd"), "ee" -> List("ff"))))

      assert(parsePhrase(dictionary, "something else") === None)
      
      assert(parsePhrase(dictionary, "{something else}") ===
        Some(Map("Unparsed" -> List("something else"))))
    }
  }

  test("Read dictionary string (multi-values)") {
    new BCatOutputParser {
      assert(parsePhrase(dictionary, "{key1:value1a,value2a}") === Some(Map("key1" -> List("value1a", "value2a"))))
    }
  }

  test("Read dictionary string (no values)") {
    new BCatOutputParser {
      assert(parsePhrase(dictionary, "{key1:}") === Some(Map("key1" -> List())))
    }
  }

  test("Multivalue") {
    new BCatOutputParser {
      assert(parsePhrase(multiValue, "hello") === Some(List("hello")))
      assert(parsePhrase(multiValue, "hello,byebye") === Some(List("hello", "byebye")))
      assert(parsePhrase(multiValue, "hello,bye bye") === Some(List("hello", "bye bye")))
      assert(parsePhrase(multiValue, " hello ,bye bye") === Some(List(" hello ", "bye bye")))

      assert(parsePhrase(multiValue, "hello,bye(bye)") === Some(List("hello", "bye(bye)")))
      assert(parsePhrase(multiValue, "hello,bye(bye,ee)") === Some(List("hello", "bye(bye,ee)")))

      assert(parsePhrase(multiValue, "hello,bye(bye,'e'e)") === Some(List("hello", "bye(bye,'e'e)")))
    }
  }

  test("Bare value - general") {
    new BCatOutputParser {
      assert(parsePhrase(bareValue, "hello") === Some("hello"))
      assert(parsePhrase(bareValue, "h-ello") === Some("h-ello"))
      assert(parsePhrase(bareValue, "he llo") === Some("he llo"))

      assert(parsePhrase(bareValue, "he(l)lo") === None)
      assert(parsePhrase(bareValue, "he[llo") === None)
      assert(parsePhrase(bareValue, "he]llo") === None)
      assert(parsePhrase(bareValue, "he}llo") === None)
      assert(parsePhrase(bareValue, "he{llo") === None)
      assert(parsePhrase(bareValue, "he<llo") === None)
      assert(parsePhrase(bareValue, "he>llo") === None)
      assert(parsePhrase(bareValue, "he'llo") === None)
      assert(parsePhrase(bareValue, "he,llo") === None)
      assert(parsePhrase(bareValue, "he:llo") === None)
    }
  }

  test("Bare value - with spaces") {
    new BCatOutputParser {
      assert(parsePhrase(value, "value1 ") === Some("value1 "))
      assert(parsePhrase(value, " value1") === Some(" value1"))
      assert(parsePhrase(value, " value1 ") === Some(" value1 "))
      assert(parsePhrase(value, " val ue ") === Some(" val ue "))
    }
  }

  test("Value") {
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

      assert(parsePhrase(value, "h (ello)") === Some("h (ello)"))

      assert(parsePhrase(value, "hel)lo") === None)
      assert(parsePhrase(value, "hel(lo") === None)
      assert(parsePhrase(value, "hello,") === None)
    }

  }

}
