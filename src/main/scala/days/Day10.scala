package se.jakub
package days

import java.util
import scala.collection.immutable.Set
import scala.collection.mutable


object Day10 extends AdventOfCode {
  private case class Node(pipe: Char, pos: Position)

  private case class Position(x: Int, y: Int)

  val fileNamePart1: String = "day10_part1.txt"
  val fileNamePart2: String = fileNamePart1

  def part1(input: List[String]): String = {
    val pipeMap = new mutable.HashMap[Position, Node]()
    val graph = new mutable.HashMap[Node, mutable.HashSet[Node]]()
    var startNode: Node = null

    for ((line, y) <- input.zipWithIndex) {
      for ((c, x) <- line.zipWithIndex) {
        val pos = Position(x, y)
        val newNode = if (c == 'S') {
          val tmp = Node('-', pos)
          startNode = tmp
          tmp
        } else {
          Node(c, pos)
        }
        pipeMap(pos) = newNode
        graph(newNode) = new mutable.HashSet[Node]()
      }
    }

    for ((currentNode, nodes) <- graph) {
      val neighbours = getNeighbours(pipeMap, currentNode)

      if (neighbours.isDefined) {
        val (posA, posB) = neighbours.get
        val nodeA = pipeMap(posA)
        val nodeB = pipeMap(posB)

        graph(nodeA) += currentNode
        graph(nodeB) += currentNode

        graph(currentNode) += nodeA
        graph(currentNode) += nodeB
      }
    }

    //    for (elem <- graph) {
    //      if (elem._1.pipe != '.') {
    //        println(elem)
    //      }
    //    }

    val queue = new util.LinkedList[(Node, Int)]()
    queue.addFirst((startNode, 0))
    val visited = mutable.HashSet[Node]()
    var maxStep = 0

    while (!queue.isEmpty) {
      val (node, steps) = queue.pollFirst()
      visited.add(node)
      maxStep = Math.max(maxStep, steps)

      for (nextNode <- graph(node)) {
        if (!visited.contains(nextNode)) {
          queue.addLast((nextNode, steps + 1))
        }
      }
    }

//    for (y <- (0 until input.size)) {
//      for (x <- (0 until input.head.length)) {
//        val pos = Position(x, y)
//        val node = pipeMap(pos)
//        if (visited.contains(node)) {
//          print(node.pipe)
//        } else {
//          print(".")
//        }
//      }
//      println()
//    }

    maxStep.toString
  }

  private def areValidNeighbours(node: Node, posA: Position, posB: Position, pipeMap: mutable.HashMap[Position, Node]): Boolean = {
    val pipeA = pipeMap(posA).pipe
    val pipeB = pipeMap(posB).pipe
    val currentPipe = node.pipe

    if (isValid(currentPipe, pipeA, pipeB)) {
      return true;
    }

    false
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

  private def getNeighbours(pipeMap: mutable.HashMap[Position, Node], node: Node): Option[(Position, Position)] = {
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

  def part2(input: List[String]): String = {
    ""
  }
}
