name := "EigenD Explorer"

version := "0.8"

// Override the default artifact name

artifactName in (Compile, packageBin) := {
  (sv: ScalaVersion, module: ModuleID, artifact: Artifact) =>
  artifact.name + "-" + module.revision + "." + artifact.extension
}

// Include libraries in the runtime jar

mappings in (Compile, packageBin) <++= baseDirectory map { base =>
  Seq(
    (base / "lib" / "config-1.0.0.jar") -> "lib/config-1.0.0.jar",
    (base / "lib" / "jline-1.0.jar")    -> "lib/jline-1.0.jar"
  )
}

// Put the library jars onto the classpath

packageOptions in (Compile, packageBin) +=
  Package.ManifestAttributes(
    java.util.jar.Attributes.Name.CLASS_PATH -> "lib/config-1.0.0.jar lib/jline-1.0.jar"
  )

// How to get a complete stack trace from scalatest
// testOptions in Test += Tests.Argument("-oF")

