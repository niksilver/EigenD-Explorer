import java.io.File
import java.util.regex.Pattern

/**
 * Omit certain classes from an assembly.
 */
object ClassOmission {
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
  
  // Whether a path (e.g. "scala\xml\Attributes.class") should be
  // omitted from the assembly
  def toRemove(s: String) =
    omPatterns exists { pat => Pattern.matches(pat, s) }
  
  def removeOmissions(s: Seq[(File,String)]): Seq[(File,String)] =
    s filterNot { fs => toRemove(fs._2) }

}