package se.jakub
package days

import scala.collection.mutable

object Day23 extends AdventOfCode {
  private type Graph = mutable.HashMap[(Int, Int), mutable.HashSet[(Int, Int, Int)]]

  val fileNamePart1: String = "day23_part1.txt"
  val fileNamePart2: String = fileNamePart1

  private val moves = Map(
    '.' -> List((1, 0), (0, 1), (-1, 0), (0, -1)),
    '<' -> List((-1, 0)),
    '>' -> List((1, 0)),
    '^' -> List((0, -1)),
    'v' -> List((0, 1)),
  )

  def part1(input: List[String]): String = {
    val graph = createGraph(input)
    val (startX, startY, endX, endY) = getStartAndEndPos(input)
    val attachedToSlope = new mutable.HashSet[(Int, Int)]()

    addEdges(input, graph, attachedToSlope)
    removeUnnecessaryEdges(input, graph, attachedToSlope)
    longestPath(startX, startY, endX, endY, graph).toString
  }

  def part2(input: List[String]): String = {
    val updatedInput = input.map(_.replaceAll("[<>v^]", "."))

    val graph = createGraph(updatedInput)
    val (startX, startY, endX, endY) = getStartAndEndPos(updatedInput)
    val attachedToSlope = new mutable.HashSet[(Int, Int)]()

    addEdges(updatedInput, graph, attachedToSlope)
    removeUnnecessaryEdges(updatedInput, graph, attachedToSlope)
    longestPath(startX, startY, endX, endY, graph).toString
  }

  private def addEdges(input: List[String], graph: Graph, attachedToSlope: mutable.HashSet[(Int, Int)]): Unit = {
    for ((pos, s) <- graph) {
      val (x, y) = (pos._1, pos._2)
      val c = input(y)(x)

      for ((dx, dy) <- moves(c)) {
        val (newX, newY) = (x + dx, y + dy)
        if (graph.contains((newX, newY))) {
          val newC = input(newY)(newX)
          
          if (newC == '.') {
            graph(x, y) += (newX, newY, 1)
            if (c != '.') {
              attachedToSlope.add((newX, newY))
            }
          } else if (newC != '#') {
            val (slopeDx, slopeDy) = moves(newC).head
            if (slopeDx == dx && slopeDy == dy) {
              graph(x, y) += (newX, newY, 1)
              attachedToSlope.add((x, y))
            }
          }
        }
      }
    }
  }

  private def getStartAndEndPos(input: List[String]): (Int, Int, Int, Int) = {
    (input.head.indexOf("."), 0, input.last.indexOf("."), input.size - 1)
  }

  private def longestPath(startX: Int, startY: Int, endX: Int, endY: Int, graph: Graph): Int = {
    val visited = new mutable.HashSet[(Int, Int)]()

    def dfs(x: Int, y: Int, s: Int): Int = {
      if (x == endX && y == endY) s
      else {
        var maxLength = -1

        visited.add((x, y))
        for ((nx, ny, ns) <- graph((x, y))) {
          if (!visited.contains(nx, ny)) {
            maxLength = maxLength max dfs(nx, ny, ns + s)
          }
        }
        visited.remove((x, y))

        maxLength
      }
    }

    dfs(startX, startY, 0)
  }

  private def removeUnnecessaryEdges(input: List[String], graph: Graph, attachedToSlope: mutable.HashSet[(Int, Int)]): Unit = {
    var found = true

    while (found) {
      found = false
      for ((x, y) <- graph.keys.toList) {
        if (shouldRemoveNode(input, x, y, attachedToSlope, graph)) {
          found = true
          removeNodeAndUpdateGraph(x, y, graph)
        }
      }
    }
  }

  private def shouldRemoveNode(input: List[String], x: Int, y: Int, attachedToSlope: mutable.HashSet[(Int, Int)], graph: Graph): Boolean = {
    input(y)(x) == '.' && graph((x, y)).size == 2 && !attachedToSlope.contains((x, y))
  }

  private def removeNodeAndUpdateGraph(x: Int, y: Int, graph: Graph): Unit = {
    val List((lx, ly, distLeft), (rx, ry, distRight)) = graph((x, y)).toList

    graph((lx, ly)).remove((x, y, distLeft))
    graph((rx, ry)).remove((x, y, distRight))

    val newDist = distLeft + distRight
    graph((lx, ly)) += ((rx, ry, newDist))
    graph((rx, ry)) += ((lx, ly, newDist))

    graph.remove((x, y))
  }

  private def createGraph(input: List[String]): Graph = {
    val graph = new mutable.HashMap[(Int, Int), mutable.HashSet[(Int, Int, Int)]]()

    for ((line, y) <- input.zipWithIndex; (c, x) <- line.zipWithIndex if c != '#') {
      graph((x, y)) = new mutable.HashSet[(Int, Int, Int)]()
    }

    graph
  }
}
