package org.pigsaw.eigendmapper

/**
 * Utility class to output and pad triples of strings with separators.
 * E.g. this sequence:
 * <pre>
 *   ("a1", "a22", "")
 *   ("b111", "b", "")
 *   ("", "c2", "c3")
 * </pre>
 * with the separator *** gets output as:
 * <pre>
 *   a1  ***a22
 *   b111***b
 *          c2 ***c3
 * </pre>
 *
 */
class Padder(data: Seq[(String, String, String)], separator: String) {
  
  private val leftPadding = data.map( _._1.length ).max
  private val midPadding  = data.map( _._2.length ).max
  private val noSeparator = " " * separator.length
  
  def output: Seq[String] = data map { d =>
    d._1.padTo(leftPadding, ' ') +
    (if (d._1 == "") noSeparator else separator) +
    d._2.padTo(midPadding, ' ') +
    (if (d._3 == "") noSeparator else separator) +
    d._3
  }
}