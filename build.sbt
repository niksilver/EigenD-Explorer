import AssemblyKeys._

// ------------------------------------------------

name := "EigenD Explorer"

version := "0.8"

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
  import java.util.regex.Pattern
  val omissions = Seq(
    "scala.xml."
  )
  val omPatterns = omissions map { pkg => pkg + ".*.class" }
  println("omissions is " + omissions)
  println("omPatterns is " + omPatterns)
  def toRemove(s: String) = {
    if (omPatterns exists { pat => Pattern.matches(pat, s) }) {
      println("Omitting "+ (s))
      true
    }
    else
      false
  }
  def removeOmissions(s: Seq[(File,String)]): Seq[(File,String)] = {
    s filterNot { fs => toRemove(fs._2) }
  }
  { f: File =>
    removeOmissions(asm(f))
  }
}

// ------------------------------------------------

// How to get a complete stack trace from scalatest
// testOptions in Test += Tests.Argument("-oF")

