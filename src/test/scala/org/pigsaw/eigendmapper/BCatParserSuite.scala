package org.pigsaw.eigendmapper

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.pigsaw.eigendmapper._
import org.pigsaw.eigendmapper.Mapper._

@RunWith(classOf[JUnitRunner])
class BCatParserSuite extends FunSuite {

  trait TestParser extends BCatParser {
    def parsePhrase[T](parser: Parser[T], dictstr: String): Option[T] =
      parseAll(parser, dictstr) match {
        case Success(out, _) => Some(out)
        case fail => None
      }
  }

  test("Read dictionary line") {
    new TestParser {
      assert(parsePhrase(outputLine, "1 hello") === Some(StateVariableLine("1", "hello")))
      assert(parsePhrase(outputLine, "12 hello") === Some(StateVariableLine("12", "hello")))
      assert(parsePhrase(outputLine, "12.34 hello") === Some(StateVariableLine("12.34", "hello")))
      assert(parsePhrase(outputLine, "12.34.56 hello") === Some(StateVariableLine("12.34.56", "hello")))
      assert(parsePhrase(outputLine, "something else") === None)
    }
  }

  test("Key-value pairs (single value)") {
    new TestParser {
      assert(parsePhrase(keyValuePairs, "key1:value1") ===
        Some(Map("key1" -> List("value1"))))

      assert(parsePhrase(keyValuePairs, "key1: value1 ") ===
        Some(Map("key1" -> List(" value1 "))))

      assert(parsePhrase(keyValuePairs, "key1:value1,key2:value2") ===
        Some(Map("key1" -> List("value1"), "key2" -> List("value2"))))
    }
  }

  test("Key-value pairs (multi-values)") {
    new TestParser {
      assert(parsePhrase(keyValuePairs, "key1:value1a,value1b") ===
        Some(Map("key1" -> List("value1a", "value1b"))))
    }
  }

  test("keyValueStringsToMap") {
    new TestParser {
      assert(keyValueStringsToMap(List("key1", ":", "value1")) === Map("key1" -> List("value1")))

      assert(keyValueStringsToMap(
        List("key1", ":", "value1", ",", "key2", ":", "value2")) === Map("key1" -> List("value1"), "key2" -> List("value2")))

      assert(keyValueStringsToMap(
        List("key1", ":", "value1a", ",", "value1b", ",", "key2", ":", "value2")) === Map("key1" -> List("value1a", "value1b"), "key2" -> List("value2")))

      assert(keyValueStringsToMap(
        List("key1", ":", "value1", ",", "key2", ":", "value2a", ",", "value2b")) === Map("key1" -> List("value1"), "key2" -> List("value2a", "value2b")))

      assert(keyValueStringsToMap(
        List("key1", ":", ",", "key2", ":", "value2")) === Map("key1" -> List(), "key2" -> List("value2")))

      assert(keyValueStringsToMap(
        List("key1", ":", "value1", ",", "key2", ":")) === Map("key1" -> List("value1"), "key2" -> List()))
    }
  }

  test("Read dictionary string (single values)") {
    new TestParser {
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
    new TestParser {
      assert(parsePhrase(dictionary, "{key1:value1a,value2a}") === Some(Map("key1" -> List("value1a", "value2a"))))
    }
  }

  test("Read dictionary string (no values)") {
    new TestParser {
      assert(parsePhrase(dictionary, "{key1:}") === Some(Map("key1" -> List())))
    }
  }

  test("Bare value - general") {
    new TestParser {
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
    new TestParser {
      assert(parsePhrase(value, "value1 ") === Some("value1 "))
      assert(parsePhrase(value, " value1") === Some(" value1"))
      assert(parsePhrase(value, " value1 ") === Some(" value1 "))
      assert(parsePhrase(value, " val ue ") === Some(" val ue "))
    }
  }

  test("Value") {
    new TestParser {
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

  test("Some real dictionary outputs") {
    new TestParser {
      assert(parsePhrase(dictionary, "{domain:aniso([]),cname:status output,protocols:output}") ===
        Some(Map(
          ("domain" -> List("aniso([])")),
          ("cname" -> List("status output")),
          ("protocols" -> List("output"))
        ))
      )

      assert(parsePhrase(dictionary, "{domain:bint(1,32,1,[]),master:,cname:preroll,protocols:input explicit output}") ===
        Some(Map(
          ("domain" -> List("bint(1,32,1,[])")),
          ("master" -> List()),
          ("cname" -> List("preroll")),
          ("protocols" -> List("input explicit output"))
        ))
      )
      
      // 

      assert(parsePhrase(dictionary, "{domain:bool([]),master:,cname:midi clock enable," +
          "protocols:input set explicit output,verbs:v(1,set([],~a,role(None,[instance(~self)])))," +
          "v(2,set([un],~a,role(None,[instance(~self)]))),v(3,set([toggle],~a,role(None,[instance(~self)])))}") ===
        Some(Map(
          ("domain" -> List("bool([])")),
          ("master" -> List()),
          ("cname" -> List("midi clock enable")),
          ("protocols" -> List("input set explicit output")),
          ("verbs" -> List("v(1,set([],~a,role(None,[instance(~self)])))",
              "v(2,set([un],~a,role(None,[instance(~self)])))",
              "v(3,set([toggle],~a,role(None,[instance(~self)])))"))
        ))
      )

    }
  }

}
