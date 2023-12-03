package se.jakub
package days

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Day3 extends AdventOfCode {
  val fileNamePart1 = "day3_part1.txt"
  val fileNamePart2 = fileNamePart1
  private val starPattern = """(\*)""".r
  private val numberPattern = """(\d+)""".r

  def part1(input: List[String]): String = {
    var answer = 0

    for ((line, rowIndex) <- input.zipWithIndex) {
      numberPattern.findAllMatchIn(line)
        .map(m => (line.substring(m.start, m.end), m.start, m.end))
        .toList
        .foreach { case (number, startIdx, endIdx) =>
          val hasAdjacentSymbol = (startIdx until endIdx)
            .flatMap(x => adjacentPositions(x, rowIndex))
            .filter { case (xPos, yPos) => isInsideBounds(input, xPos, yPos) && !input(yPos).charAt(xPos).isDigit }
            .distinct
            .exists { case (xPos, yPos) => !input(yPos).charAt(xPos).equals('.') }
          if (hasAdjacentSymbol) {
            answer += number.toInt
          }
        }
    }

    answer.toString
  }

  def part2(input: List[String]): String = {
    val gears = mutable.HashMap.empty[(Int, Int), ListBuffer[Int]]

    for ((line, rowIndex) <- input.zipWithIndex) {
      starPattern.findAllMatchIn(line).foreach { m =>
        gears += ((m.start, rowIndex) -> mutable.ListBuffer())
      }
    }

    for ((line, rowIndex) <- input.zipWithIndex) {
      numberPattern.findAllMatchIn(line)
        .map(m => (line.substring(m.start, m.end), m.start, m.end))
        .foreach { case (number, startIdx, endIdx) => (startIdx until endIdx)
          .flatMap(x => adjacentPositions(x, rowIndex))
          .filter { case (xPos, yPos) => isInsideBounds(input, xPos, yPos) && !input(yPos).charAt(xPos).isDigit }
          .filter { case (xPos, yPos) => input(yPos).charAt(xPos).equals('*') }
          .distinct
          .foreach { case (xPos, yPos) => gears((xPos, yPos)) += number.toInt }
        }
    }

    gears.values
      .filter(_.length == 2)
      .map(l => BigInt(l.head) * BigInt(l.last))
      .sum
      .toString()
  }

  private def isInsideBounds(input: List[String], xPos: Int, yPos: Int) = {
    xPos >= 0 && yPos >= 0 && yPos < input.length && xPos < input(yPos).length
  }

  private def adjacentPositions(x: Int, y: Int): Seq[(Int, Int)] = {
    for {
      dx <- -1 to 1
      dy <- -1 to 1
      if !(dx == 0 && dy == 0)
    } yield (x + dx, y + dy)
  }
}
