package org.pigsaw.eigendmapper

import scala.sys.process._

object testing {
  val bin = "C:\\Program Files (x86)\\Eigenlabs\\release-2.0.68-stable\\bin"
                                                  //> bin  : java.lang.String = C:\Program Files (x86)\Eigenlabs\release-2.0.68-st
                                                  //| able\bin
  //Process(bin + "/bls.exe <main>").lines
  "abcd".tail.init                                //> res0: String = bc
}