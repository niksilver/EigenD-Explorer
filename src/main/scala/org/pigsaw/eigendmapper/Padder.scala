package org.pigsaw.eigendmapper

/**
 * Utility class to output and pad triples of strings with separators.
 * E.g. this sequence:
 * {{{
 *   ("a1", "a22", "")
 *   ("b111", "b", "")
 *   ("", "c2", "c3")
 * }}}
 * with the separator *** gets output as:
 * {{{
 *   a1  ***a22
 *   b111***b
 *          c2 ***c3
 * }}}
 *
 */
class Padder(data: Seq[(String, String, String)], separator: String) {

  private val leftPadding = data.map(_._1.length).max
  private val midPadding = data.map(_._2.length).max
  private val noSeparator = " " * separator.length

  def output: Seq[String] = data map { d =>
    d._1.padTo(leftPadding, ' ') +
      (if (d._1 == "") noSeparator else separator) +
      d._2.padTo(midPadding, ' ') +
      (if (d._3 == "") noSeparator else separator) +
      d._3
  }
}

/**
 * Calculate padding needed for three columns.
 * The object is created with the total number of columns and
 * the minimum width of each column.
 * Then given particular sizes for each column (its average and
 * max) it returns the recommended size for each column.
 * The recommended sizes will be no less than the minimum for
 * each column, ideally allowing no line breaks, but if necessary
 * resizing according to the columns average length.
 */
class PadCalc(cols: Int, minLeft: Int, minMid: Int, minRight: Int) {

  def forSizes(avgLeft: Int, avgMid: Int, avgRight: Int,
    maxLeft: Int, maxMid: Int, maxRight: Int): (Int, Int, Int) = {

    val practicalMinLeft = minLeft min maxLeft
    def incLeft(inc: Double): Double =
      (inc + practicalMinLeft) min maxLeft

    val practicalMinMid = minMid min maxMid
    def incMid(inc: Double): Double =
      (inc + practicalMinMid) min maxMid

    val practicalMinRight = minRight min maxRight
    def incRight(inc: Double): Double =
      (inc + practicalMinRight) min maxRight

    val avgTotal = avgLeft + avgMid + avgRight
    val scaleLeft = avgLeft.toDouble / avgTotal
    val scaleMid = avgMid.toDouble / avgTotal
    val scaleRight = avgRight.toDouble / avgTotal

    def settle(inc: Double): Seq[Int] = {
      val cand = Seq(incLeft(inc * scaleLeft), incMid(inc * scaleMid), incRight(inc * scaleRight))
      val candInt = cand map { _.round.toInt }
      val total = (cand(0) + cand(1) + cand(2)).round.toInt
      if (total == cols
        || (candInt(0) == maxLeft && candInt(1) == maxMid && candInt(2) == maxRight))
        candInt
      else
        settle(inc + 1.0)
    }
    
    val answer = settle(0)
    (answer(0), answer(1), answer(2))
  }

  def forSizesHalfFinished(avgLeft: Int, avgMid: Int, avgRight: Int,
    maxLeft: Int, maxMid: Int, maxRight: Int): (Int, Int, Int) = {

    // Pass 1: Try to fit in all the text, but don't make any
    // columns smaller than their min
    val pass1 = Seq(maxLeft max minLeft, maxMid max minMid, maxLeft max minRight)

    // Pass 2: If the total is too much then shrink everything proportional
    // to (a) the total max cols allowed, and (b) its weighted average.
    val pass1Tot = pass1(0) + pass1(1) + pass1(2)
    val overShoot = pass1Tot - cols
    val avgTot = avgLeft + avgMid + avgRight
    val pass2 =
      if (overShoot <= 0)
        pass1
      else
        Seq(pass1(0) - (avgTot - overShoot * avgLeft / avgTot),
          pass1(1) - (avgTot - overShoot * avgMid / avgTot),
          pass1(2) - (avgTot - overShoot * avgRight / avgTot))
    (pass2(0), pass2(1), pass2(2))
  }
}