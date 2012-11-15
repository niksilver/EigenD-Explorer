package org.pigsaw.eigendmapper

import scala.sys.process._

object testing {
  val bin = "C:\\Program Files (x86)\\Eigenlabs\\release-2.0.68-stable\\bin"
                                                  //> bin  : java.lang.String = C:\Program Files (x86)\Eigenlabs\release-2.0.68-st
                                                  //| able\bin
  val lines = Process(bin + "/bcat.exe <metronome1>").lines.toList
                                                  //> log:using portbase 55555
                                                  //| lines  : List[String] = List(. {cname:metronome,cordinal:1,protocols:browse 
                                                  //| metronome,plugin:metronome,version:2.0.68-stable,cversion:1.0.0,verbs:v(1,st
                                                  //| art([],None)),v(2,stop([],None)),v(3,start([toggle],None)),v(200,set([],~a,r
                                                  //| ole(None,[partof(~s),notproto(set)]),role(to,[abstract]))),v(201,set([],~a,r
                                                  //| ole(None,[partof(~s),notproto(set)]))),v(202,set([un],~a,role(None,[partof(~
                                                  //| s),notproto(set)]))),v(203,up([],~a,role(None,[partof(~s),notproto(up)]))),v
                                                  //| (204,down([],~a,role(None,[partof(~s),notproto(down)]))),v(205,set([toggle],
                                                  //| ~a,role(None,[partof(~s),notproto(set)]))),v(206,up([],~a,role(None,[partof(
                                                  //| ~s),notproto(up)]),role(by,[numeric]))),v(207,down([],~a,role(None,[partof(~
                                                  //| s),notproto(down)]),role(by,[numeric]))),timestamp:8058323431}, 1 {cname:out
                                                  //| puts,protocols:}, 1.1 {domain:aniso([]),cname:bar beat output,protocols:outp
                                                  //| ut,slave:'<clicker1>#2.2','<rig1>#3.1','<rig2>#3.1','<rig3>#3.1','<rig5>#3.
                                                  //| Output exceeds cutoff limit.
  //for (line <- lines) yield { line } toList
}