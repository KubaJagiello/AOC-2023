package se.jakub
package days

object Day11 extends AdventOfCode {
  private type Grid = List[List[Char]]
  val fileNamePart1: String = "day11_part1.txt"
  val fileNamePart2: String = fileNamePart1

  def part1(input: List[String]): String = sumDistancesBetweenGalaxies(input, 2L).toString

  private def sumDistancesBetweenGalaxies(input: List[String], emptyFactor: Long): Long = {
    val grid = input.map(_.toList)
    val (emptyRows, emptyColumns) = getListOfEmptyRowsAndColumns(grid)

    findAllPairs(getPositionsOfGalaxies(grid))
      .map((a, b) =>
        manhattanDist(a, b, emptyRowsIndices(emptyRows, a, b), emptyColumnsIndices(emptyColumns, a, b), emptyFactor)
      )
      .sum
  }

  private def findAllPairs(positions: List[Position]): List[(Position, Position)] = {
    for {
      (pos1, index1) <- positions.zipWithIndex
      (pos2, index2) <- positions.zipWithIndex if index1 < index2
    } yield (pos1, pos2)
  }

  private def getPositionsOfGalaxies(grid: Grid): List[Position] = {
    (for {
      y <- grid.indices
      x <- grid.head.indices
      if grid(y)(x) == '#'
    } yield Position(x, y)).toList
  }

  private def getListOfEmptyRowsAndColumns(grid: Grid): (List[Int], List[Int]) = {
    val emptyRows = grid.indices.filter(y => !grid(y).contains('#')).toList
    val emptyColumns = grid.head.indices.filter(x => !grid.exists(row => row(x) == '#')).toList

    (emptyRows, emptyColumns)
  }

  private def emptyRowsIndices(emptyRows: List[Int], posA: Position, posB: Position): Int = {
    emptyRows.count(y => y >= Math.min(posA.y, posB.y) && y < Math.max(posA.y, posB.y))
  }

  private def emptyColumnsIndices(emptyColumns: List[Int], posA: Position, posB: Position): Int = {
    emptyColumns.count(x => x >= Math.min(posA.x, posB.x) && x < Math.max(posA.x, posB.x))
  }

  private def manhattanDist(pos1: Position, pos2: Position, emptyRows: Int, emptyColumns: Int, factor: Long): Long = {
    val distance = (Math.abs(pos1.x - pos2.x) + Math.abs(pos1.y - pos2.y)).toLong
    distance + (emptyRows * (factor - 1L)) + (emptyColumns * (factor - 1L))
  }

  def part2(input: List[String]): String = sumDistancesBetweenGalaxies(input, 1000000L).toString

  private case class Position(x: Int, y: Int)
}
