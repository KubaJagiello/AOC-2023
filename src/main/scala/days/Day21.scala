package se.jakub
package days

import scala.collection.mutable

object Day21 extends AdventOfCode {
  val fileNamePart1: String = "day21_part1.txt"
  val fileNamePart2: String = fileNamePart1

  def part1(input: List[String]): String = solve(input, 64).count(_._2 % 2 == 0).toString

  def part2(input: List[String]): String = {
    val m = solve(input, input.head.length * 2 + 1)
    val even_squares = m.count(_._2 % 2 == 0).toLong
    val odd_squares = m.count(_._2 % 2 == 1).toLong
    val even_corners = m.filter(_._2 % 2 == 0).count(_._2 > 65).toLong
    val odd_corners = m.filter(_._2 % 2 == 1).count(_._2 > 65).toLong
    val n = ((26501365 - 65) / 131).toLong

    val answer = ((n + 1) * (n + 1) * odd_squares)
      + (n * n * even_squares)
      - ((n + 1) * odd_corners)
      + (n * even_corners)

    answer.toString
  }

  private def solve(input: List[String], c: Int): Map[(Int, Int), Int] = {
    val moves = List((1, 0), (0, 1), (-1, 0), (0, -1))
    val memory = new mutable.HashMap[(Int, Int), Int]()
    val q = mutable.Queue[(Int, Int, Int)]()
    val (startX, startY) = (input.head.length / 2, input.length / 2)
    q.enqueue((startX, startY, 0))

    while (q.nonEmpty) {
      val (x, y, step) = q.dequeue()

      if (!memory.contains((x, y)) && step <= c) {
        memory((x, y)) = step

        for ((dx, dy) <- moves) {
          val (nx, ny) = (x + dx, y + dy)
          if (valid(nx, ny, input) && input(ny)(nx) != '#') {
            q.enqueue((nx, ny, step + 1))
          }
        }
      }
    }
    memory.toMap
  }

  private def valid(x: Int, y: Int, input: List[String]): Boolean = x >= 0 && y >= 0 && x < input.head.length && y < input.length
}
