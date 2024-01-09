package se.jakub
package days

import scala.collection.mutable.ListBuffer

object Day9 extends AdventOfCode {
  val fileNamePart1: String = "day9_part1.txt"
  val fileNamePart2: String = fileNamePart1

  def part1(input: List[String]): String = {
    input
      .foldLeft(0L) { (acc, line) =>
        val diffs = getHistoryOfDifferences(line.split(" ").map(_.toLong).to(ListBuffer))

        for (i <- 1 until diffs.length) {
          diffs(i) += (diffs(i).last + diffs(i - 1).last)
        }

        acc + diffs.last.last
      }
      .toString
  }

  def part2(input: List[String]): String = {
    input
      .foldLeft(0L) { (acc, line) =>
        val diffs = getHistoryOfDifferences(line.split(" ").map(_.toLong).to(ListBuffer).reverse)

        for (i <- 1 until diffs.length) {
          diffs(i) += (diffs(i).last + diffs(i - 1).last)
        }

        acc + diffs.last.last
      }
      .toString
  }

  private def getHistoryOfDifferences(numbers: ListBuffer[Long]) =
    Iterator
      .iterate(numbers)(n => getDifferences(n))
      .takeWhile(diffs => !onlyZeros(diffs))
      .toList
      .reverse

  private def onlyZeros(numbers: ListBuffer[Long]): Boolean = !numbers.exists(n => n != 0L)

  private def getDifferences(numbers: ListBuffer[Long]): ListBuffer[Long] = {
    val diffs = for {
      idx <- 1 until numbers.length
    } yield numbers(idx) - numbers(idx - 1)

    diffs.to(ListBuffer)
  }
}
