package se.jakub
package days

import scala.collection.mutable.ListBuffer

object Day13 extends AdventOfCode {
  val fileNamePart1: String = "day13_part1.txt"
  val fileNamePart2: String = fileNamePart1

  private def cmpStrings(a: String, b: String): Boolean = a == b

  private def cmpStringWithSmudge(a: String, b: String): Boolean = countDifferences(a, b) == 1

  def part1(input: List[String]): String = {
    parseMaps(input).map(line => findMirrorsPart1(line.toList)).sum.toString
  }

  def part2(input: List[String]): String = {
    parseMaps(input).map(line => findMirrorsPart2(line.toList)).sum.toString
  }

  private def calculateAnswer(rowsIndices: List[(Int, Int)], colsIndices: List[(Int, Int)]): Int = {
    if (rowsIndices.nonEmpty) {
      100 * (rowsIndices.head._1 + 1)
    } else {
      colsIndices.head._1 + 1
    }
  }

  private def findMirrorsPart1(m: List[String]): Int = {
    val (perfectRowIndices, perfectColIndices) = getPerfectIndices(m)

    calculateAnswer(perfectRowIndices, perfectColIndices)
  }

  private def findMirrorsPart2(m: List[String]): Int = {
    val (perfectRowIndices, perfectColIndices) = getPerfectIndices(m)
    val rowIndicesWithSmudges = mirroredRowsIndices(m, cmpStringWithSmudge)
      .concat(mirroredRowsIndices(m, cmpStrings))
      .filter((a, b) => !perfectRowIndices.contains((a, b)) && onlyOneSmudgedRow(m, a, b))
    val colIndicesWithSmudges = mirroredColumnsIndices(m, cmpStringWithSmudge)
      .concat(mirroredColumnsIndices(m, cmpStrings))
      .filter((a, b) => !perfectColIndices.contains((a, b)) && onlyOneSmudgeColumn(m, a, b))

    calculateAnswer(rowIndicesWithSmudges, colIndicesWithSmudges)
  }

  private def getPerfectIndices(m: List[String]): (List[(Int, Int)], List[(Int, Int)]) = {
    val rowIndices = mirroredRowsIndices(m, cmpStrings)
      .filter((a, b) => hasPerfectMirroredRows(m, a, b))
    val colIndices = mirroredColumnsIndices(m, cmpStrings)
      .filter((a, b) => hasPerfectMirroredColumns(m, a, b))

    (rowIndices, colIndices)
  }

  private def hasPerfectMirroredRows(m: List[String], i: Int, j: Int): Boolean = {
    val (rowsAbove, rowsBelow) = getRowsAboveAndBelow(m, i, j)
    rowsAbove.zip(rowsBelow).forall((rowAbove, rowBelow) => rowAbove == rowBelow)
  }

  private def onlyOneSmudgedRow(m: List[String], i: Int, j: Int): Boolean = {
    val (rowsAbove, rowsBelow) = getRowsAboveAndBelow(m, i, j)
    rowsAbove.zip(rowsBelow)
      .map((rowAbove, rowBelow) => countDifferences(rowAbove, rowBelow))
      .sum <= 1
  }

  private def getRowsAboveAndBelow(m: List[String], i: Int, j: Int): (List[String], List[String]) = {
    (m.take(i + 1).reverse, m.drop(j))
  }

  private def hasPerfectMirroredColumns(m: List[String], i: Int, j: Int): Boolean = {
    val (colsLeft, colsRight) = getColsLeftAndRight(m, i, j)
    colsLeft.zip(colsRight).forall((left, right) => left == right)
  }

  private def onlyOneSmudgeColumn(m: List[String], i: Int, j: Int): Boolean = {
    val (colsLeft, colsRight) = getColsLeftAndRight(m, i, j)
    colsLeft.zip(colsRight).map((left, right) => countDifferences(left, right))
      .sum < 2
  }

  private def getColsLeftAndRight(m: List[String], i: Int, j: Int): (List[String], List[String]) = {
    val columns = (0 until m.head.length).map(x => getColumnAsString(x, m))
    (columns.take(i + 1).reverse.toList, columns.drop(j).toList)
  }

  private def countDifferences(str1: String, str2: String): Int = {
    str1.zip(str2).count((char1, char2) => char1 != char2)
  }

  private def mirroredRowsIndices(m: List[String], cmpFunc: (String, String) => Boolean): List[(Int, Int)] = {
    (1 until m.size).flatMap(y =>
      if (cmpFunc(m(y), m(y - 1))) Some((y - 1, y)) else None
    ).toList
  }

  private def mirroredColumnsIndices(m: List[String], cmpFunc: (String, String) => Boolean): List[(Int, Int)] = {
    (1 until m.head.length).flatMap { x =>
      if (cmpFunc(getColumnAsString(x - 1, m), getColumnAsString(x, m))) Some((x - 1, x)) else None
    }.toList
  }

  private def getColumnAsString(x: Int, m: List[String]): String = {
    m.map(row => row.charAt(x)).mkString
  }

  private def parseMaps(input: List[String]): ListBuffer[ListBuffer[String]] = {
    input.foldLeft(ListBuffer(ListBuffer[String]())) { (acc, line) =>
      if (line.isEmpty) {
        acc += ListBuffer[String]()
      } else {
        acc.last += line
      }
      acc
    }
  }
}
