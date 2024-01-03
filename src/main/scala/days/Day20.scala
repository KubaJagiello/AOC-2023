package se.jakub
package days


import scala.collection.mutable
import scala.collection.mutable.HashMap

object Day20 extends AdventOfCode {
  val fileNamePart1: String = "day20_part1.txt"
  val fileNamePart2: String = fileNamePart1

  private class Graph {
    private val adjacencyList: mutable.HashMap[String, List[Node]] = mutable.HashMap()
    private val nodeMap: mutable.HashMap[String, Node] = mutable.HashMap()

    def addNode(node: Node): Unit = {
      nodeMap(node.id) = node
      adjacencyList.getOrElseUpdate(node.id, List())
    }

    def addEdge(nodeA: Node, nodeB: Node): Unit = {
      adjacencyList(nodeA.id) = nodeB :: adjacencyList.getOrElse(nodeA.id, List())
      addNode(nodeA)
      addNode(nodeB)
    }

    def getNeighbours(id: String): List[Node] = {
      adjacencyList(id)
    }

    def getNode(id: String): Node = {
      if(!nodeMap.contains(id)) {
        addNode(Untyped(id))
      }
      nodeMap(id)
    }
  }

  private trait Node {
    def id: String
  }

  private case class FlipFlop(id: String, var on: Boolean = false) extends Node

  private case class Broadcaster(id: String) extends Node

  private case class Untyped(id: String) extends Node

  private case class Conjunction(id: String, inputsMemory: mutable.HashMap[String, Pulse] = mutable.HashMap()) extends Node {
    def addInputToMemory(node: Node, pulse: Pulse): Unit = {
      inputsMemory(node.id) = pulse
    }

    def onlyHighPulses(): Boolean = inputsMemory.forall(_._2.high)
  }

  private case class Pulse(high: Boolean)

  private val LOW = 0
  private val HIGH = 1

  def part1(input: List[String]): String = {
    val graph = parseInput(input)
    val queue = mutable.Queue[(Node, Node, Int)]()
    val answer = Array(0, 0)

    for( _ <- (0 until 1000)) {
      val startNode = graph.getNode("broadcaster")
      queue.enqueue((startNode, startNode, LOW))

      while (queue.nonEmpty) {
        val (currentNode, fromNode, power) = queue.dequeue()
        answer(power) += 1

        currentNode match {
          case f: FlipFlop => {
            if (power == LOW) {
              for (neighbour <- graph.getNeighbours(f.id)) {
                queue.enqueue((neighbour, currentNode, if (f.on) LOW else HIGH))
              }
              f.on = !f.on
            }
          }
          case c: Conjunction => {
            c.addInputToMemory(fromNode, if (power == LOW) Pulse(false) else Pulse(true))

            for (neighbour <- graph.getNeighbours(c.id)) {
              queue.enqueue((neighbour, currentNode, if (c.onlyHighPulses()) LOW else HIGH))
            }
          }
          case b: Broadcaster => {
            for (neighbour <- graph.getNeighbours(b.id)) {
              queue.enqueue((neighbour, currentNode, power))
            }
          }
          case _ =>
        }
      }

    }

    println(s"${answer(0)} ${answer(1)}")
    (answer(0) * answer(1)).toString
  }

  private def parseInput(lines: List[String]): Graph = {
    val graph = new Graph()

    for (line <- lines) {
      graph.addNode(getNode(line.split(" -> ")(0)))
    }

    for (line <- lines) {
      val s = line.split(" -> ")
      for (n <- s(1).split(", ").toList) {
        val leftNode = graph.getNode(leftStrip(s(0)))
        val rightNode = graph.getNode(n)

        rightNode match {
          case conjunction: Conjunction => conjunction.addInputToMemory(leftNode, Pulse(false))
          case _ =>
        }
        graph.addEdge(leftNode, rightNode)
      }
    }

    graph
  }

  private def leftStrip(s: String): String = {
    if (s.contains("%") || s.contains("&")) {
      s.substring(1)
    } else {
      s
    }
  }

  private def getNode(s: String): Node = {
    if (s.contains("%")) {
      FlipFlop(leftStrip(s))
    } else if (s.contains("&")) {
      Conjunction(leftStrip(s))
    } else {
      Broadcaster(leftStrip(s))
    }
  }

  def part2(input: List[String]): String = {
    ""
  }
}
