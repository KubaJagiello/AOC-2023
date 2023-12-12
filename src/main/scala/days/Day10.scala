package se.jakub
package days

import scala.collection.immutable.Set
import scala.collection.mutable


object Day10 extends AdventOfCode {
  private case class Node(pipe: Char, pos: Position)

  private case class Position(x: Int, y: Int)

  val fileNamePart1: String = "day10_part1.txt"
  val fileNamePart2: String = fileNamePart1

  def part1(input: List[String]): String = {
    val pipeMap = new mutable.HashMap[Position, Char]()
    val (graph, startNode) = createGraph(input, pipeMap)
    (getPath(startNode, graph).size / 2).toString
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

  def part2(input: List[String]): String = {
    val pipeMap = new mutable.HashMap[Position, Char]()
    val (graph, startNode) = createGraph(input, pipeMap)
    val visited = getPath(startNode, graph)

    replaceUnusedPipes(input, visited, pipeMap)
    countInside(input, pipeMap).toString
  }

  private def createGraph(input: List[String], pipeMap: mutable.HashMap[Position, Char]): (mutable.HashMap[Node, mutable.HashSet[Node]], Node) = {
    val graph = new mutable.HashMap[Node, mutable.HashSet[Node]]()
    var startNode: Node = null

    for ((line, y) <- input.zipWithIndex) {
      for ((c, x) <- line.zipWithIndex) {
        val pos = Position(x, y)
        val pipe = if (c == 'S') {
          startNode = Node('-', pos)
          '-'
        } else {
          c
        }
        pipeMap(pos) = pipe
        graph(Node(pipe, pos)) = new mutable.HashSet[Node]()
      }
    }
    connectNodesInGraph(graph, pipeMap)

    (graph, startNode)
  }

  private def connectNodesInGraph(graph: mutable.HashMap[Node, mutable.HashSet[Node]], pipeMap: mutable.HashMap[Position, Char]): Unit = {
    for ((currentNode, nodes) <- graph) {
      val neighbours = getNeighbours(pipeMap, currentNode)

      if (neighbours.isDefined) {
        val (posA, posB) = neighbours.get
        val nodeA = Node(pipeMap(posA), posA)
        val nodeB = Node(pipeMap(posB), posB)

        graph(nodeA) += currentNode
        graph(nodeB) += currentNode

        graph(currentNode) += nodeA
        graph(currentNode) += nodeB
      }
    }
  }

  private def getPath(startNode: Node, graph: mutable.HashMap[Node, mutable.HashSet[Node]]): Set[Position] = {
    val queue = mutable.Queue[(Node, Int)]((startNode, 0))
    val visited = mutable.HashSet[Position]()

    while (queue.nonEmpty) {
      val (node, steps) = queue.dequeue()
      visited += node.pos

      graph(node).foreach { nextNode =>
        if (!visited.contains(nextNode.pos)) {
          queue.enqueue((nextNode, steps + 1))
        }
      }
    }
    visited.toSet
  }

  private def areValidNeighbours(node: Node, posA: Position, posB: Position, pipeMap: mutable.HashMap[Position, Char]): Boolean = {
    isValid(node.pipe, pipeMap(posA), pipeMap(posB))
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

  private def getNeighbours(pipeMap: mutable.HashMap[Position, Char], node: Node): Option[(Position, Position)] = {
    val (posA, posB) = node.pipe match {
      case '|' => (Position(node.pos.x, node.pos.y - 1), Position(node.pos.x, node.pos.y + 1))
      case '-' => (Position(node.pos.x - 1, node.pos.y), Position(node.pos.x + 1, node.pos.y))
      case 'L' => (Position(node.pos.x, node.pos.y - 1), Position(node.pos.x + 1, node.pos.y))
      case 'J' => (Position(node.pos.x, node.pos.y - 1), Position(node.pos.x - 1, node.pos.y))
      case 'F' => (Position(node.pos.x, node.pos.y + 1), Position(node.pos.x + 1, node.pos.y))
      case '7' => (Position(node.pos.x, node.pos.y + 1), Position(node.pos.x - 1, node.pos.y))
      case _ => (Position(-1337, -1337), Position(-1337, -1337))
    }

    if (pipeMap.contains(posA) && pipeMap.contains(posB) && areValidNeighbours(node, posA, posB, pipeMap)) {
      Some(posA, posB)
    } else {
      None
    }
  }
}
