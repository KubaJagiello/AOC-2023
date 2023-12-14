package se.jakub
package days

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.control.Breaks.*

object Day14 extends AdventOfCode {
  private case class Direction(dx: Int, dy: Int)

  val fileNamePart1: String = "day14_part1.txt"
  val fileNamePart2: String = fileNamePart1
  private type Grid = List[Array[Char]]
  private val ROUND_ROCK = 'O'
  private val CUBE_ROCK = '#'
  private val EMPTY = '.'

  object Direction extends Enumeration {
    val North, East, South, West = Value

    def getDeltas(direction: Direction.Value): (Int, Int) = direction match {
      case North => (0, -1)
      case East => (1, 0)
      case South => (0, 1)
      case West => (-1, 0)
    }
  }

  def part1(input: List[String]): String = {
    val grid = input.map(_.toArray)
    shift(grid, Direction.North)
    calculateAnswer(grid).toString
  }

  def part2(input: List[String]): String = {
    val grid = input.map(_.toArray)
    val cache = new mutable.HashMap[String, Long]()
    val maxCycles: Long = 1_000_000_000L

    var idx = 1L
    var cycleFound = false

    while (idx < maxCycles) {
      shift(grid, Direction.North)
      shift(grid, Direction.West)
      shift(grid, Direction.South)
      shift(grid, Direction.East)

      val gridAsStr = gridToString(grid)
      if (cache.contains(gridAsStr) && !cycleFound) {
        val cycleLength = idx - cache(gridAsStr)
        idx += ((maxCycles - idx) / cycleLength) * cycleLength
        cycleFound = true
      } else {
        cache(gridAsStr) = idx
        idx += 1
      }
    }

    calculateAnswer(grid).toString
  }

  private def gridToString(grid: Grid): String = grid.foldLeft("")((acc, arr) => acc + arr.mkString)

  private def calculateAnswer(grid: Grid): Int = {
    grid.zipWithIndex.foldLeft(0) { case (acc, (line, i)) => acc + (line.count(_.equals(ROUND_ROCK)) * (grid.length - i)) }
  }

  private def shift(grid: Grid, direction: Direction.Value): Unit = {
    val xIndices = grid.head.indices
    val yIndices = grid.indices

    direction match {
      case Direction.West =>
        yIndices.foreach(y => xIndices.foreach(x => if (grid(y)(x) == ROUND_ROCK) move(grid, x, y, direction)))
      case Direction.East =>
        yIndices.foreach(y => xIndices.reverse.foreach(x => if (grid(y)(x) == ROUND_ROCK) move(grid, x, y, direction)))
      case Direction.North =>
        xIndices.foreach(x => yIndices.foreach(y => if (grid(y)(x) == ROUND_ROCK) move(grid, x, y, direction)))
      case Direction.South =>
        xIndices.foreach(x => yIndices.reverse.foreach(y => if (grid(y)(x) == ROUND_ROCK) move(grid, x, y, direction)))
    }
  }

  private def move(grid: Grid, x: Int, y: Int, direction: Direction.Value): Unit = {
    def canMove(grid: Grid, x: Int, y: Int): Boolean = grid(y)(x) == EMPTY

    def isValid(grid: Grid, x: Int, y: Int): Boolean = x >= 0 && x < grid.head.length && y >= 0 && y < grid.length

    val (dx, dy) = Direction.getDeltas(direction)

    @tailrec
    def move(prevX: Int, prevY: Int): Unit = {
      val newX = prevX + dx
      val newY = prevY + dy
      if (isValid(grid, newX, newY) && canMove(grid, newX, newY)) {
        grid(prevY)(prevX) = EMPTY
        grid(newY)(newX) = ROUND_ROCK
        move(newX, newY)
      }
    }

    move(x, y)
  }
}
