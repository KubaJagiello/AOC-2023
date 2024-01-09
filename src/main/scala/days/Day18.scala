package se.jakub
package days

import java.lang.Integer.parseInt
import java.lang.Math.abs

object Day18 extends AdventOfCode {
  private type Trench = List[((Int, Int), Long)]
  val fileNamePart1: String = "day18_part1.txt"
  val fileNamePart2: String = fileNamePart1
  private val dir = Map("U" -> (0, -1), "D" -> (0, 1), "L" -> (-1, 0), "R" -> (1, 0))

  def part1(input: List[String]): String = {
    val parsedData = parsePart1(input)
    getArea(getCords(parsedData), getBoundary(parsedData)).toString
  }

  private def getArea(cords: List[(Long, Long)], boundary: Long): Long = {
    val area = cords.zipWithIndex.foldLeft(0L) { case (acc, (x, i)) =>
      acc + x._1 * (cords(mod(i - 1, cords.length))._2 - cords((i + 1) % cords.length)._2)
    }
    boundary + ((abs(area) / 2) - (boundary / 2) + 1)
  }

  private def mod(dividend: Int, divisor: Int): Int = {
    val result = dividend % divisor
    if (result < 0) result + divisor else result
  }

  private def getCords(inputTuple: Trench): List[(Long, Long)] = {
    inputTuple.foldLeft(List[(Long, Long)]((0L, 0L))) { case (acc, ((dx, dy), steps)) =>
      val (lastX, lastY) = acc.last
      acc :+ (lastX + dx * steps, lastY + dy * steps)
    }
  }

  private def getBoundary(inputTuple: Trench): Long = inputTuple.map(_._2).sum

  private def parsePart1(input: List[String]): Trench = input.map(s => s.split(" ")).map(s => (dir(s(0)), s(1).toInt))

  def part2(input: List[String]): String = {
    val parsedData = parsePart2(input)
    getArea(getCords(parsedData), getBoundary(parsedData)).toString
  }

  private def parsePart2(input: List[String]): Trench = {
    val t = Map(0 -> "R", 1 -> "D", 2 -> "L", 3 -> "U")
    input
      .map(s => s.split(" "))
      .map(s => (s(2)(s(2).length - 2).toString.toInt, s(2).substring(2, s(2).length - 1)))
      .map((d, c) => (dir(t(d)), parseInt(c.substring(0, c.length - 1), 16)))
  }
}
