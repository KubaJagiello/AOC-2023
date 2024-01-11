package se.jakub
package days

import org.jgrapht.alg.flow.EdmondsKarpMFImpl
import org.jgrapht.graph.{DefaultEdge, SimpleGraph}

import scala.jdk.CollectionConverters.*

object Day25 extends AdventOfCode {
  val fileNamePart1: String = "day25_part1.txt"
  val fileNamePart2: String = fileNamePart1

  def part1(input: List[String]): String = {
    val graph = getGraph(input)
    val minCutAlg: EdmondsKarpMFImpl[String, DefaultEdge] = new EdmondsKarpMFImpl(graph)
    val vertices = graph.vertexSet().stream().toList.asScala.toList

    var i = vertices.size - 1
    while (minCutAlg.calculateMinCut(vertices.head, vertices(i)) != 3) {
      i -= 1
    }

    (minCutAlg.getSourcePartition.size * minCutAlg.getSinkPartition.size).toString
  }

  def part2(input: List[String]): String = ""

  private def getGraph(input: List[String]): SimpleGraph[String, DefaultEdge] = {
    val graph: SimpleGraph[String, DefaultEdge] = new SimpleGraph(classOf[DefaultEdge])

    for (line <- input) {
      val aNode = line.split(": ").head
      graph.addVertex(aNode)

      for (bNode <- line.split(": ").last.split(" ")) {
        graph.addVertex(bNode)
        graph.addEdge(aNode, bNode)
        graph.addEdge(bNode, aNode)
      }
    }

    graph
  }
}
