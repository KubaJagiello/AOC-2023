package se.jakub
package days

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Day15 extends AdventOfCode {
  val fileNamePart1: String = "day15_part1.txt"
  val fileNamePart2: String = fileNamePart1

  def part1(input: List[String]): String = {
    input.head.split(",").map(hash).sum.toString
  }

  def part2(input: List[String]): String = {
    val boxes = List.fill(256)(ListBuffer[String]())
    val mem = new mutable.HashMap[(Int, String), Int]()

    input.head.split(",").foreach { s =>
      val (label, focalLength) = getLens(s)
      val boxIndex = hash(label)
      val box = boxes(boxIndex)

      if (s.contains("-")) {
        box -= label
        mem.remove((boxIndex, label))
      } else {
        if (!box.contains(label)) {
          box += label
        }
        mem((boxIndex, label)) = focalLength
      }
    }

    boxes.zipWithIndex.foldLeft(0L) { case (acc, (box, i)) =>
      box.zipWithIndex.foldLeft(acc) { case (innerAcc, (elem, j)) => innerAcc + (i + 1) * (j + 1) * mem((i, elem)) }
    }.toString
  }

  private def getLens(s: String): (String, Int) = {
    val parts = s.split("[=\\-]")
    (parts(0), if (parts.length > 1) parts(1).toInt else 0)
  }

  private def hash(line: String): Int = line.foldLeft(0)((hash, char) => ((hash + char.toInt) * 17) % 256)
}
