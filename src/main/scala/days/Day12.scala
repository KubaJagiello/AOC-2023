package se.jakub
package days

import scala.collection.parallel.CollectionConverters.*

object Day12 extends AdventOfCode {
  val fileNamePart1: String = "day12_part1.txt"
  val fileNamePart2: String = fileNamePart1

  type Group = List[Int]
  type DP = Array[Array[Array[Long]]]

  def part1(input: List[String]): String = {
    input.par
      .map(line => (line.split(" ")(0), parseNumbers(line.split(" ")(1))))
      .map((row, numbers) => noOfArrangements(row, numbers))
      .sum.toString
  }

  def part2(input: List[String]): String = {
    input.par
      .map(line => (line.split(" ")(0), line.split(" ")(1)))
      .map { (row, numbers) =>
        val repeatedRows = List.fill(5)(row).mkString("?")
        val repeatedNumbers = List.fill(5)(parseNumbers(numbers)).flatten
        noOfArrangements(repeatedRows, repeatedNumbers)
      }.sum.toString
  }

  private def createMem(rowLength: Int, groupLength: Int): DP = {
    val dp = Array.ofDim[Long](rowLength + 1, groupLength + 1, rowLength + 1)
    for {
      i <- dp.indices
      j <- dp(i).indices
      k <- dp(i)(j).indices
    } dp(i)(j)(k) = -1
    dp
  }

  private def noOfArrangements(row: String, group: Group): Long = {
    val dp = createMem(row.length, group.length)

    def recurse(idx: Int, groupIdx: Int, currentGroupSize: Int): Long = {
      if (idx == row.length) {
        if ((groupIdx == group.length - 1 && group(groupIdx) == currentGroupSize) ||
          (groupIdx == group.length && currentGroupSize == 0)) {
          return 1
        }
        return 0
      }

      if (dp(idx)(groupIdx)(currentGroupSize) != -1) {
        return dp(idx)(groupIdx)(currentGroupSize)
      }

      var sum: Long = 0
      val character = row.charAt(idx)

      if (character == '?' || character == '#') {
        sum += recurse(idx + 1, groupIdx, currentGroupSize + 1)
      }
      if (character == '?' || character == '.') {
        if (currentGroupSize > 0 && groupIdx < group.length &&
          group(groupIdx) == currentGroupSize) {
          sum += recurse(idx + 1, groupIdx + 1, 0)
        }
        if (currentGroupSize == 0) {
          sum += recurse(idx + 1, groupIdx, 0)
        }
      }

      dp(idx)(groupIdx)(currentGroupSize) = sum
      sum
    }

    recurse(0, 0, 0)
  }

  private def parseNumbers(line: String): Group = line.split(",").map(_.toInt).toList
}
