package se.jakub
package days

import java.lang.Math.max
import scala.collection.mutable

object Day2 extends AdventOfCode {
  val fileNamePart1 = "day2_part1.txt"
  val fileNamePart2 = fileNamePart1
  private val cubesPattern = """(\d+) red|(\d+) blue|(\d+) green""".r
  private val idPattern = """Game (\d+):""".r
  private val red = "red"
  private val green = "green"
  private val blue = "blue"

  def part1(lines: List[String]): String = {
    val colorToValue = Map[String, Int](red -> 12, green -> 13, blue -> 14)

    lines
      .foldLeft(0) { (acc, line) =>
        val gameId = idPattern.findFirstMatchIn(line).map(_.group(1)).get.toInt
        val invalidGame = cubesPattern
          .findAllIn(line)
          .toList
          .map(_.split(" ").toList)
          .map(elem => (elem.head.toInt, elem.last))
          .exists { case (value, color) => value > colorToValue(color) }
        acc + (if (invalidGame) 0 else gameId)
      }
      .toString
  }

  def part2(lines: List[String]): String = {
    lines
      .foldLeft(0) { (acc, line) =>
        val map = cubesPattern
          .findAllIn(line)
          .toList
          .map(_.split(" ").toList)
          .map(elem => (elem.head.toInt, elem.last))
          .foldLeft(mutable.HashMap[String, Int](red -> 0, green -> 0, blue -> 0)) { case (acc, (value, color)) =>
            acc.update(color, max(acc(color), value))
            acc
          }

        acc + map(red) * map(green) * map(blue)
      }
      .toString
  }
}
