package se.jakub
package days

import scala.collection.immutable.Set
import scala.collection.mutable


object Day10 extends AdventOfCode {

  private type PipeMap = mutable.HashMap[Position, Char]

  private case class Position(x: Int, y: Int)

  val fileNamePart1: String = "day10_part1.txt"
  val fileNamePart2: String = fileNamePart1


  def part1(input: List[String]): String = {
    val (graph, pipeMap, startPosition) = createGraph(input)
    (getPath(startPosition, graph).size / 2).toString
  }

  def part2(input: List[String]): String = {
    val (graph, pipeMap, startPosition) = createGraph(input)
    val visited = getPath(startPosition, graph)

    replaceUnusedPipes(input, visited, pipeMap)
    countInside(input, pipeMap).toString
  }

  private def findStartPosition(input: List[String]): Position = {
    input.zipWithIndex
      .flatMap((line, y) => line.zipWithIndex.collect { case ('S', x) => Position(x, y) })
      .head
  }

  private def replaceUnusedPipes(input: List[String], visitedPath: Set[Position], pipeMap: PipeMap): Unit = {
    for (y <- input.indices) {
      for (x <- 0 until input.head.length) {
        val pos = Position(x, y)

        if (!visitedPath.contains(pos)) {
          pipeMap(pos) = '.'
        }
      }
    }
  }

  private def countInside(input: List[String], pipeMap: PipeMap): Int = {
    input.indices.map { y =>
      val (count, _) = input.head.indices.foldLeft((0, false)) { case ((innerCount, inside), x) =>
        pipeMap(Position(x, y)) match {
          case p if Set('J', 'L', '|').contains(p) => (innerCount, !inside)
          case p if Set('-', 'F', '7').contains(p) => (innerCount, inside)
          case _ if inside => (innerCount + 1, inside)
          case _ => (innerCount, inside)
        }
      }
      count
    }.sum
  }

  private def createGraph(input: List[String]): (Map[Position, Set[Position]], PipeMap, Position) = {
    val startPosition = findStartPosition(input)
    val pipeMap = new mutable.HashMap[Position, Char]()
    val graph = input.zipWithIndex.flatMap { case (line, y) =>
      line.zipWithIndex.map { case (pipe, x) =>
        val pos = Position(x, y)
        pipeMap(pos) = pipe
        pos -> Set.empty[Position]
      }
    }.toMap

    val mutableGraph = collection.mutable.Map(graph.toSeq: _*)
    pipeMap(startPosition) = '-'

    mutableGraph.keys.foreach { currentPos =>
      getNeighbours(pipeMap, currentPos).foreach { case (posA, posB) =>
        mutableGraph(posA) = mutableGraph(posA) + currentPos
        mutableGraph(posB) = mutableGraph(posB) + currentPos
        mutableGraph(currentPos) = mutableGraph(currentPos) + posA + posB
      }
    }

    (mutableGraph.toMap, pipeMap, startPosition)
  }

  private def getPath(startPosition: Position, graph: Map[Position, Set[Position]]): Set[Position] = {
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

  private def isValid(currentPipe: Char, pipeA: Char, pipeB: Char): Boolean = {
    val validCombinations = Map(
      '|' -> (Set('F', '|', '7'), Set('L', 'J', '|')),
      '-' -> (Set('-', 'F', 'L'), Set('-', 'J', '7')),
      'L' -> (Set('|', 'F', '7'), Set('-', 'J', '7')),
      'J' -> (Set('|', 'F', '7'), Set('-', 'F', 'L')),
      'F' -> (Set('|', 'L', 'J'), Set('-', 'J', '7')),
      '7' -> (Set('|', 'L', 'J'), Set('-', 'F', 'L'))
    )

    validCombinations.get(currentPipe) match {
      case Some((validA, validB)) => validA.contains(pipeA) && validB.contains(pipeB)
      case None => false
    }
  }

  private def getNeighbours(pipeMap: PipeMap, currentPos: Position): Option[(Position, Position)] = {
    def adjacentPosition(direction: Char): Option[Position] = direction match {
      case 'U' => Some(Position(currentPos.x, currentPos.y - 1))
      case 'D' => Some(Position(currentPos.x, currentPos.y + 1))
      case 'L' => Some(Position(currentPos.x - 1, currentPos.y))
      case 'R' => Some(Position(currentPos.x + 1, currentPos.y))
      case _ => None
    }

    val (dirA, dirB) = pipeMap(currentPos) match {
      case '|' => ('U', 'D')
      case '-' => ('L', 'R')
      case 'L' => ('U', 'R')
      case 'J' => ('U', 'L')
      case 'F' => ('D', 'R')
      case '7' => ('D', 'L')
      case _ => ('X', 'X')
    }

    for {
      posA <- adjacentPosition(dirA)
      posB <- adjacentPosition(dirB)
      if pipeMap.contains(posA) && pipeMap.contains(posB)
      if isValid(pipeMap(currentPos), pipeMap(posA), pipeMap(posB))
    } yield (posA, posB)
  }
}
