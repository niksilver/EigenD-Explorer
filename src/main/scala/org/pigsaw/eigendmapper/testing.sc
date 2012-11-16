package org.pigsaw.eigendmapper

import scala.sys.process._

object testing {
  val bin = "C:\\Program Files (x86)\\Eigenlabs\\release-2.0.68-stable\\bin"
                                                  //> bin  : java.lang.String = C:\Program Files (x86)\Eigenlabs\release-2.0.68-st
                                                  //| able\bin
  //val lines = Process(bin + "/bcat.exe <metronome1>").lines.toList
  
  val myMap = Map("one" -> List(1, 11), "two" -> List(2, 22))
                                                  //> myMap  : scala.collection.immutable.Map[java.lang.String,List[Int]] = Map(on
                                                  //| e -> List(1, 11), two -> List(2, 22))
  
  /*for {
    twoList <- myMap.get("two") getOrElse List()
    twoElem <- twoList
  } yield twoElem*/
  
}