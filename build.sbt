import AssemblyKeys._

// ------------------------------------------------

name := "EigenD Explorer"

version := "0.8"

unmanagedResourceDirectories in (Compile) <+= baseDirectory ({ base =>
  base / "LICENCES"
})

//dependencyClasspath in assembly <<= (dependencyClasspath in assembly, baseDirectory) map { (dcp, bd) =>
//  println("** " + dcp.files)
//  println("** " + bd)
//  val out = dcp :+ Attributed.blank(bd / "LICENCE" / "jline-licence.txt")
//  println("@@ " + out.files)
//  out
//}
//dependencyClasspath <++= baseDirectory map { base =>
//  Seq(
//     (base / "LICENCE"),
//     (base / "LICENCE" / "jline-licence.txt")
//  )
//}


// ------------------------------------------------

// For sbt-assembly

assemblySettings

// During testing only, skip the Scala jar
// assembleArtifact in packageScala := false

// Skip the test during assembly

test in assembly := {}

// Exclude certain jars from assembly

excludedJars in assembly <<= (fullClasspath in assembly) map { cp => 
  cp filter { c =>
    (c.data.getName contains "scalatest") ||
    (c.data.getName contains "junit")
  }
}

jarName in assembly <<= (artifact, version) { (artifact, version) =>
  artifact.name + "-" + version + ".jar"
}

//excludedFiles in assembly <<= (excludedFiles in assembly) { ef =>
//  { sf: Seq[java.io.File] => println("** " + sf); sf }
//}

// ------------------------------------------------

// Include libraries and licence info in the runtime jar

//assembledMappings in assembly <<= (assembledMappings in assembly, baseDirectory) map { (a, b) =>
//  { f: java.io.File =>
//    //....
//  }
//  Seq(
//    // (base / "lib" / "config-1.0.0.jar") -> "lib/config-1.0.0.jar",
//    // (base / "lib" / "jline-1.0.jar")    -> "lib/jline-1.0.jar",
//    (base / "LICENSES" / "jline-licence.txt" ) -> "LICENSES/jline-licence.txt",
//    (base / "LICENSES" / "config-licence.txt" ) -> "LICENSES/config-licence.txt",
//    (base / "LICENSES" / "eigend-explorer-licence.txt" ) -> "LICENSES/eigend-explorer-licence.txt"
//  )
//}

mappings in (Compile, packageBin) <++= baseDirectory map { base =>
  Seq(
    // (base / "lib" / "config-1.0.0.jar") -> "lib/config-1.0.0.jar",
    // (base / "lib" / "jline-1.0.jar")    -> "lib/jline-1.0.jar",
    (base / "LICENcES" / "jline-licence.txt" ) -> "LICENcES/jline-licence.txt",
    (base / "LICENcES" / "config-licence.txt" ) -> "LICENcES/config-licence.txt",
    (base / "LICENcES" / "eigend-explorer-licence.txt" ) -> "LICENcES/eigend-explorer-licence.txt"
  )
}

// How to get a complete stack trace from scalatest
// testOptions in Test += Tests.Argument("-oF")

