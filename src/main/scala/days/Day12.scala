package se.jakub
package days

import scala.collection.mutable
import scala.collection.parallel.CollectionConverters.*

object Day12 extends AdventOfCode {
  val fileNamePart1: String = "day12_part1.txt"
  val fileNamePart2: String = fileNamePart1

  def part1(input: List[String]): String = {
    input.par
      .map(line => (line.split(" ")(0), parseNumbers(line.split(" ")(1))))
      .map((row, numbers) => noOfArrangements(row, numbers))
      .sum
      .toString
  }

  private def noOfArrangements(row: String, group: List[Int]): Long = {
    val cache = new mutable.HashMap[State, Long]()

    def dfsArrangements(idx: Int, groupIdx: Int, currentGroupSize: Int): Long = {
      val state = State(idx, groupIdx, currentGroupSize)
      if (cache.get(state).exists(_ != 1337)) {
        return cache(state)
      }

      if (idx == row.length) {
        if (
          (groupIdx == group.length - 1 && group(groupIdx) == currentGroupSize) ||
          (groupIdx == group.length && currentGroupSize == 0)
        ) {
          return 1
        }
        return 0
      }

      var arrangements = 0L
      val character = row.charAt(idx)
      if (Set('?', '.').contains(character)) {
        if (groupIdx < group.length && group(groupIdx) == currentGroupSize) {
          arrangements += dfsArrangements(idx + 1, groupIdx + 1, 0)
        }
        if (currentGroupSize == 0) {
          arrangements += dfsArrangements(idx + 1, groupIdx, 0)
        }
      }
      if (Set('?', '#').contains(character)) {
        arrangements += dfsArrangements(idx + 1, groupIdx, currentGroupSize + 1)
      }

      cache(state) = arrangements
      arrangements
    }

    dfsArrangements(0, 0, 0)
  }

  private def parseNumbers(line: String): List[Int] = line.split(",").map(_.toInt).toList

  def part2(input: List[String]): String = {
    input.par
      .map(line => (line.split(" ")(0), line.split(" ")(1)))
      .map { (row, numbers) =>
        val repeatedRows = List.fill(5)(row).mkString("?")
        val repeatedNumbers = List.fill(5)(parseNumbers(numbers)).flatten
        noOfArrangements(repeatedRows, repeatedNumbers)
      }
      .sum
      .toString
  }

  private case class State(idx: Int, groupIdx: Int, currentGroupSize: Int)
}
