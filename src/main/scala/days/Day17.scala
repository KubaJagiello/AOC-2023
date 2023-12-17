package se.jakub
package days

import scala.collection.mutable

object Day17 extends AdventOfCode {
  private type Grid = List[List[Char]]
  val fileNamePart1: String = "day17_part1.txt"
  val fileNamePart2: String = fileNamePart1

  def part1(input: List[String]): String = solve(input.map(_.toList), 0, 3).toString

  def part2(input: List[String]): String = solve(input.map(_.toList), 4, 10).toString

  private def solve(grid: Grid, minSteps: Int, maxSteps: Int): Int = {
    val tupleOrdering: Ordering[(Int, Int, Int, Int, Int, Int)] = Ordering.by(_._1)
    val queue = mutable.PriorityQueue.empty(tupleOrdering.reverse)
    val visited = mutable.Set[(Int, Int, Int, Int, Int)]()

    queue.enqueue((0, 0, 0, 1, 0, 0))
    queue.enqueue((0, 0, 0, 0, 1, 0))

    while (true) {
      val (heat, x, y, dx, dy, steps) = queue.dequeue()

      if (finish(grid, minSteps, x, y, steps)) {
        return heat
      }

      if (!visited.contains((x, y, dx, dy, steps))) {
        visited.add((x, y, dx, dy, steps))
        if (canMoveStraight(grid, maxSteps, x, y, dx, dy, steps)) {
          queue.enqueue(getTuple(grid, heat, x, y, dx, dy, steps))
        }
        if (steps >= minSteps) {
          for ((ndx, ndy) <- List((1, 0), (0, 1), (-1, 0), (0, -1))) {
            if (canTurn(grid, x, y, dx, dy, ndx, ndy)) {
              queue.enqueue(getTuple(grid, heat, x, y, ndx, ndy, 0))
            }
          }
        }
      }
    }

    1_3_3_7
  }

  private def getTuple(grid: Grid, h: Int, x: Int, y: Int, dx: Int, dy: Int, steps: Int) = (h + getHeat(grid, x + dx, y + dy), x + dx, y + dy, dx, dy, steps + 1)

  private def canMoveStraight(grid: Grid, maxSteps: Int, x: Int, y: Int, dx: Int, dy: Int, steps: Int) = steps < maxSteps && isInsideGrid(grid, x + dx, y + dy)

  private def canTurn(grid: Grid, x: Int, y: Int, dx: Int, dy: Int, ndx: Int, ndy: Int) = !((dx, dy) == (ndx, ndy)) && !((dx, dy) == (-ndx, -ndy)) && isInsideGrid(grid, x + ndx, y + ndy)

  private def finish(grid: Grid, minSteps: Int, x: Int, y: Int, steps: Int) = x == grid.head.length - 1 && y == grid.size - 1 && steps >= minSteps

  private def getHeat(grid: Grid, x: Int, y: Int): Int = grid(y)(x).toString.toInt

  private def isInsideGrid(grid: Grid, x: Int, y: Int): Boolean = x >= 0 && y >= 0 && x < grid.head.length && y < grid.size
}
