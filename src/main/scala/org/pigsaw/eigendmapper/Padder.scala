/*
 *  Copyright 2012 Nik Silver.
 *  
 *  This file is part of EigenD Explorer.
 *
 *  EigenD Explorer is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  EigenD Explorer is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with EigenD Explorer.  If not, see <http://www.gnu.org/licenses/>.
 */

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
class Padder(data: Seq[(String, String, String)],
    separator: String, padCalc: PadCalc = DefaultPadCalc) {

  private val maxLeft = data.map(_._1.length).max
  private val maxMid = data.map(_._2.length).max
  private val maxRight = data.map(_._3.length).max
  
  private val avgLeft = (data map { _._1.length.toDouble } sum) / data.length
  private val avgMid = (data map { _._2.length.toDouble } sum) / data.length
  private val avgRight = (data map { _._3.length.toDouble } sum) / data.length
  
  /**
   * Widths of the main three columns.
   */
  val (padLeft, padMid, padRight) =
    padCalc.forSizes(avgLeft, avgMid, avgRight, maxLeft, maxMid, maxRight)
  
  private val noSeparator = " " * separator.length

  /**
   * Add a column of strings to an existing column of strings
   * @param curr  The current column of strings
   * @param next  The new column of Strings
   * @param padNext  Whether to pad the new column with spaces.
   */
  private def addCol(curr: Seq[String], next: Seq[String], padNext: Boolean): Seq[String] = {
    val currPad = " " * (curr map { _.length } max)
    val nextPad =
      if (padNext)
        " " * (next map { _.length } max)
      else
        ""
    curr.zipAll(next, currPad, nextPad) map { pair => pair._1 + pair._2 }
  }
  
  /**
   * Split a string into a sequence of strings of the given width.
   */
  private def split(s: String, width: Int): Seq[String] = {
    if (s == "" || width == 0)
      Seq("")
    else
      s.grouped(width).toSeq
  }
  
  /**
   * Split a string into a sequence of strings of the given width
   * and ensure the last line is padded with spaces.
   */
  private def splitAndPad(s: String, width: Int): Seq[String] =
    split(s, width) map { _.padTo(width, ' ') }
    
  /**
   * Split left, mid and right strings into a series of triples,
   * each of which is no bigger than the padding for that column.
   */
  def wrap(left: String, mid: String, right: String): List[(String, String, String)] = {
    val splitLeft = left.grouped(padLeft).toList
    val splitMid = mid.grouped(padMid).toList
    val splitLeftMid = splitLeft.zipAll(splitMid, "", "")
    
    val splitRight = right.grouped(padRight).toList
    val splitLeftMidPairRight = splitLeftMid.zipAll(splitRight, ("", ""), "")
    val splitLeftMidRight = splitLeftMidPairRight map { e => (e._1._1, e._1._2, e._2) }
    
    splitLeftMidRight
  }
  
  def output: Seq[String] = data flatMap { d =>
    val col1 = splitAndPad(d._1, padLeft)
    val col2 = if (d._1 == "") Seq(noSeparator) else Seq(separator)
    val col3 = splitAndPad(d._2, padMid)
    val col4 = if (d._3 == "") Seq("") else Seq(separator)
    val col5 = split(d._3, padRight)
    
    val col12 = addCol(col1, col2, true)
    val col123 = addCol(col12, col3, true)
    val col1234 = addCol(col123, col4, false)
    val col12345 = addCol(col1234, col5, false)
    col12345
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

  def forSizes(avgLeft: Double, avgMid: Double, avgRight: Double,
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
    val scaleLeft = avgLeft / avgTotal
    val scaleMid = avgMid / avgTotal
    val scaleRight = avgRight / avgTotal

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

/**
 * The default PadCalc. The number of columns is the console
 * columns less 1 (to avoid overrunning), less two times
 * `" --> "` which is 5 chars.
 */
object DefaultPadCalc extends
  PadCalc(Config.consoleCols.get - 1 - 10, 15,20,15)
