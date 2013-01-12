import AssemblyKeys._

// ------------------------------------------------

name := "EigenD Explorer"

version := "0.8.1"

unmanagedResourceDirectories in (Compile) <+= baseDirectory ({ base =>
  base / "LICENCES"
})


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

assembledMappings in assembly <<= (assembledMappings in assembly) map { asm =>
  { f: File =>
    ClassOmission.removeOmissions(asm(f))
  }
}

// ------------------------------------------------

// How to get a complete stack trace from scalatest
testOptions in Test += Tests.Argument("-oF")

