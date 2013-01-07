import AssemblyKeys._

// ------------------------------------------------

name := "EigenD Explorer"

version := "0.8"

unmanagedResources in Compile <++= baseDirectory map { base =>
  Seq(
     (base / "LICENCE" / "jline-licence.txt")
  )
}

// ------------------------------------------------

// For sbt-assembly

assemblySettings

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

//mappings in (Compile, packageBin) <++= baseDirectory map { base =>
//  Seq(
//    // (base / "lib" / "config-1.0.0.jar") -> "lib/config-1.0.0.jar",
//    // (base / "lib" / "jline-1.0.jar")    -> "lib/jline-1.0.jar",
//    (base / "LICENSES" / "jline-licence.txt" ) -> "LICENSES/jline-licence.txt",
//    (base / "LICENSES" / "config-licence.txt" ) -> "LICENSES/config-licence.txt",
//    (base / "LICENSES" / "eigend-explorer-licence.txt" ) -> "LICENSES/eigend-explorer-licence.txt"
//  )
//}

// How to get a complete stack trace from scalatest
// testOptions in Test += Tests.Argument("-oF")

