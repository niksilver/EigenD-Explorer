package org.pigsaw.eigendmapper

import scala.sys.process._

object testing {
  val bin = "C:\\Program Files (x86)\\Eigenlabs\\release-2.0.68-stable\\bin"
                                                  //> bin  : java.lang.String = C:\Program Files (x86)\Eigenlabs\release-2.0.68-st
                                                  //| able\bin
  //Process(bin + "/bls.exe <main>").lines
  "abcd".tail.init                                //> res0: String = bc
 
  "a(..)d".r findFirstIn("abcd")                  //> res1: Option[String] = Some(abcd)
  
  "a(..)d".r unapplySeq("abcd")                   //> res2: Option[List[String]] = Some(List(bc))
 
  val words = List("abcd", "axxd", "ayyd")        //> words  : List[java.lang.String] = List(abcd, axxd, ayyd)
  words flatMap ("a(..)d".r unapplySeq(_)) flatten//> res3: List[String] = List(bc, xx, yy)
  val re = "a(..)d".r                             //> re  : scala.util.matching.Regex = a(..)d
  val re(inner) = "abcd"                          //> inner  : String = bc
  "[^\\[\\]]+".r findFirstIn "afd[b]c"            //> res4: Option[String] = Some(afd)
}