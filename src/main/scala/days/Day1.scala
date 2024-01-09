package se.jakub
package days

import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

object Day1 extends AdventOfCode {
  val fileNamePart1 = "day1_part1.txt"
  val fileNamePart2 = fileNamePart1

  def part1(lines: List[String]): String = lines
    .foldLeft(0) { (acc, line) =>
      acc + (line.find(_.isDigit).get.toString + line.findLast(_.isDigit).get.toString).toInt
    }
    .toString

  def part2(lines: List[String]): String = {
    val map = HashMap[String, Int](
      "one" -> 1,
      "two" -> 2,
      "three" -> 3,
      "four" -> 4,
      "five" -> 5,
      "six" -> 6,
      "seven" -> 7,
      "eight" -> 8,
      "nine" -> 9
    )

    lines
      .foldLeft(0) { (acc, line) =>
        var numberAndIndexTuples = ListBuffer.empty[(Int, Int)]

        map.keys.foreach { sub =>
          Iterator
            .iterate(line.indexOf(sub)) { idx => if (idx != -1) line.indexOf(sub, idx + sub.length) else -1 }
            .takeWhile(_ != -1)
            .foreach(idx => numberAndIndexTuples += ((map(sub), idx)))
        }

        val firstDigit = line.find(_.isDigit)
        val lastDigit = line.findLast(_.isDigit)

        numberAndIndexTuples += ((firstDigit.get.toString.toInt, line.indexOf(firstDigit.get)))
        numberAndIndexTuples += ((lastDigit.get.toString.toInt, line.lastIndexOf(lastDigit.get)))
        numberAndIndexTuples = numberAndIndexTuples.sortBy(_._2)

        acc + numberAndIndexTuples.head._1 * 10 + numberAndIndexTuples.last._1
      }
      .toString
  }
}
