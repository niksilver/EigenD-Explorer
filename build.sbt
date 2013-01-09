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
  import java.util.regex.Pattern
  val omissions = Seq(
    "scala.actors.",
    "scala.concurrent.",
    "scala.parallel.",
    "scala.swing.",
    "scala.testing.",
    "scala.text.",
    "scala.util.automata.",
    "scala.util.continuations.",
    "scala.util.grammar.",
    "scala.util.logging.",
    "scala.util.regexp."
  )
  val omPatterns = omissions map { pkg => pkg + ".*.class" }
  def toRemove(s: String) =
    omPatterns exists { pat => Pattern.matches(pat, s) }
  def removeOmissions(s: Seq[(File,String)]): Seq[(File,String)] =
    s filterNot { fs => toRemove(fs._2) }
  { f: File =>
    removeOmissions(asm(f))
  }
}

// ------------------------------------------------

// How to get a complete stack trace from scalatest
// testOptions in Test += Tests.Argument("-oF")

