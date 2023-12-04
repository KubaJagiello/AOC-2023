package se.jakub
package days

import java.lang.Math.pow


object Day4 extends AdventOfCode {
  private val numberPattern = """(\d+)""".r
  val fileNamePart1 = "day4_part1.txt"
  val fileNamePart2 = fileNamePart1

  def part1(input: List[String]): String = {
    input.foldLeft(0) { (acc, line) =>
      val numbers = numberPattern.findAllMatchIn(line)
        .map(x => x.toString().toInt)
        .toList

      val commonNumbers = numbers.slice(1, 11).intersect(numbers.slice(11, numbers.size))
      if (commonNumbers.nonEmpty) acc + pow(2, commonNumbers.length - 1).toInt else acc
    }.toString
  }

  def part2(input: List[String]): String = {
    val cards = Array.fill(input.length)(1)

    for ((line, idx) <- input.zipWithIndex) {
      val numbers = numberPattern.findAllMatchIn(line)
        .map(x => x.toString().toInt)
        .toList

      val commonNumbers = numbers.slice(1, 11).intersect(numbers.slice(11, numbers.size))

      for (i <- 1 to commonNumbers.length) {
        if (idx + i < cards.length) {
          cards(idx + i) += cards(idx)
        }
      }
    }
    cards.sum.toString
  }
}
