package se.jakub
package days


import scala.collection.mutable

object Day16 extends AdventOfCode {
  private type Grid = List[List[Char]]
  private type Beam = (Int, Int, Int, Int)
  val fileNamePart1: String = "day16_part1.txt"
  val fileNamePart2: String = fileNamePart1

  def part1(input: List[String]): String = numberOfEnergizedTiles(input.map(_.toList), (-1, 0, 1, 0)).toString

  def part2(input: List[String]): String = {
    val grid = input.map(_.toList)
    var maxValue = 0

    for (y <- grid.indices) {
      maxValue = Math.max(maxValue, numberOfEnergizedTiles(grid, (-1, y, 1, 0)))
      maxValue = Math.max(maxValue, numberOfEnergizedTiles(grid, (grid.head.length, y, -1, 0)))
    }

    for (x <- grid.head.indices) {
      maxValue = Math.max(maxValue, numberOfEnergizedTiles(grid, (x, -1, 0, 1)))
      maxValue = Math.max(maxValue, numberOfEnergizedTiles(grid, (x, grid.length, 0, -1)))
    }

    maxValue.toString
  }

  private def numberOfEnergizedTiles(grid: Grid, beam: Beam): Int = {
    val queue = mutable.Queue[Beam](beam)
    val cache = mutable.Set[Beam]()

    while (queue.nonEmpty) {
      val (oldX, oldY, dx, dy) = queue.dequeue()
      val (x, y) = (dx + oldX, oldY + dy)

      if (!cache.contains((x, y, dx, dy)) && isInsideGrid(grid, x, y)) {
        cache += ((x, y, dx, dy))
        val c = grid(y)(x)

        if (isEmptySpace(grid, x, y, dx, dy)) {
          queue.enqueue((x, y, dx, dy))
        } else if (c == '/') {
          queue.enqueue((x, y, -dy, -dx))
        } else if (c == '\\') {
          queue.enqueue((x, y, dy, dx))
        } else if (c == '|' && dx != 0) {
          queue.enqueue((x, y, dy, dx))
          queue.enqueue((x, y, dy, -dx))
        } else if (c == '-' && dy != 0) {
          queue.enqueue((x, y, dy, dx))
          queue.enqueue((x, y, -dy, dx))
        }
      }
    }

    cache.map((x, y, _, _) => (x, y)).toSet.size
  }

  private def isEmptySpace(grid: Grid, x: Int, y: Int, dx: Int, dy: Int): Boolean = {
    grid(y)(x) == '.' || (dx != 0 && grid(y)(x) == '-') || (dy != 0 && grid(y)(x) == '|')
  }

  private def isInsideGrid(grid: Grid, x: Int, y: Int): Boolean = x >= 0 && y >= 0 && x < grid.head.length && y < grid.size
}

