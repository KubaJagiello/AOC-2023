package se.jakub
package days

import scala.collection.immutable.Set
import scala.collection.mutable


object Day10 extends AdventOfCode {

  private case class Position(x: Int, y: Int)

  val fileNamePart1: String = "day10_part1.txt"
  val fileNamePart2: String = fileNamePart1

  def part1(input: List[String]): String = {
    val pipeMap = new mutable.HashMap[Position, Char]()
    val startPosition = findStartPosition(input)
    val graph = createGraph(input, pipeMap, startPosition)
    (getPath(startPosition, graph).size / 2).toString
  }

  def part2(input: List[String]): String = {
    val pipeMap = new mutable.HashMap[Position, Char]()
    val startPosition = findStartPosition(input)
    val graph = createGraph(input, pipeMap, startPosition)
    val visited = getPath(startPosition, graph)

    replaceUnusedPipes(input, visited, pipeMap)
    countInside(input, pipeMap).toString
  }

  private def findStartPosition(input: List[String]): Position = {
    input.zipWithIndex
      .flatMap((line, y) => line.zipWithIndex.collect { case ('S', x) => Position(x, y) })
      .head
  }

  private def replaceUnusedPipes(input: List[String], visitedPath: Set[Position], pipeMap: mutable.HashMap[Position, Char]): Unit = {
    for (y <- input.indices) {
      for (x <- 0 until input.head.length) {
        val pos = Position(x, y)

        if (!visitedPath.contains(pos)) {
          pipeMap(pos) = '.'
        }
      }
    }
  }

  private def countInside(input: List[String], pipeMap: mutable.HashMap[Position, Char]): Int = {
    var count = 0
    for (y <- input.indices) {
      var inside = false
      for (x <- 0 until input.head.length) {
        val pipe = pipeMap(Position(x, y))

        if (Set('J', 'L', '|').contains(pipe)) {
          inside = !inside
        } else if (Set('-', 'F', '7').contains(pipe)) {

        } else if (inside) {
          count += 1
        }
      }
    }
    count
  }

  private def createGraph(input: List[String],
                          pipeMap: mutable.HashMap[Position, Char],
                          startPosition: Position): mutable.HashMap[Position, mutable.HashSet[Position]] = {
    val graph = new mutable.HashMap[Position, mutable.HashSet[Position]]()
    for ((line, y) <- input.zipWithIndex) {
      for ((c, x) <- line.zipWithIndex) {
        val pos = Position(x, y)
        pipeMap(pos) = c
        graph(pos) = new mutable.HashSet[Position]()
      }
    }
    pipeMap(startPosition) = '-'
    connectNodesInGraph(graph, pipeMap)
    graph
  }

  private def connectNodesInGraph(graph: mutable.HashMap[Position, mutable.HashSet[Position]], pipeMap: mutable.HashMap[Position, Char]): Unit = {
    graph.keys.foreach { currentPos =>
      getNeighbours(pipeMap, currentPos).foreach { case (posA, posB) =>
        graph(posA) += currentPos
        graph(posB) += currentPos
        graph(currentPos) += posA
        graph(currentPos) += posB
      }
    }
  }

  private def getPath(startPosition: Position, graph: mutable.HashMap[Position, mutable.HashSet[Position]]): Set[Position] = {
    val queue = mutable.Queue[(Position, Int)]((startPosition, 0))
    val visited = mutable.HashSet[Position]()

    while (queue.nonEmpty) {
      val (pos, steps) = queue.dequeue()
      visited += pos

      graph(pos).foreach { nextPos =>
        if (!visited.contains(nextPos)) {
          queue.enqueue((nextPos, steps + 1))
        }
      }
    }
    visited.toSet
  }

  private def areValidNeighbours(currentPos: Position, posA: Position, posB: Position, pipeMap: mutable.HashMap[Position, Char]): Boolean = {
    isValid(pipeMap(currentPos), pipeMap(posA), pipeMap(posB))
  }

  private def isValid(currentPipe: Char, pipeA: Char, pipeB: Char): Boolean = {
    if (currentPipe == '|' && Set('F', '|', '7').contains(pipeA) && Set('L', 'J', '|').contains(pipeB)) {
      return true
    } else if (currentPipe == '-' && Set('-', 'F', 'L').contains(pipeA) && Set('-', 'J', '7').contains(pipeB)) {
      return true
    } else if (currentPipe == 'L' && Set('|', 'F', '7').contains(pipeA) && Set('-', 'J', '7').contains(pipeB)) {
      return true
    } else if (currentPipe == 'J' && Set('|', 'F', '7').contains(pipeA) && Set('-', 'F', 'L').contains(pipeB)) {
      return true
    } else if (currentPipe == 'F' && Set('|', 'L', 'J').contains(pipeA) && Set('-', 'J', '7').contains(pipeB)) {
      return true
    } else if (currentPipe == '7' && Set('|', 'L', 'J').contains(pipeA) && Set('-', 'F', 'L').contains(pipeB)) {
      return true
    }
    false
  }

  private def getNeighbours(pipeMap: mutable.HashMap[Position, Char], currentPos: Position): Option[(Position, Position)] = {
    val (posA, posB) = pipeMap(currentPos) match {
      case '|' => (Position(currentPos.x, currentPos.y - 1), Position(currentPos.x, currentPos.y + 1))
      case '-' => (Position(currentPos.x - 1, currentPos.y), Position(currentPos.x + 1, currentPos.y))
      case 'L' => (Position(currentPos.x, currentPos.y - 1), Position(currentPos.x + 1, currentPos.y))
      case 'J' => (Position(currentPos.x, currentPos.y - 1), Position(currentPos.x - 1, currentPos.y))
      case 'F' => (Position(currentPos.x, currentPos.y + 1), Position(currentPos.x + 1, currentPos.y))
      case '7' => (Position(currentPos.x, currentPos.y + 1), Position(currentPos.x - 1, currentPos.y))
      case _ => (Position(-1337, -1337), Position(-1337, -1337))
    }

    if (pipeMap.contains(posA) && pipeMap.contains(posB) && areValidNeighbours(currentPos, posA, posB, pipeMap)) {
      Some(posA, posB)
    } else {
      None
    }
  }
}
