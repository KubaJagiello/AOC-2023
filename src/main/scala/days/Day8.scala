package se.jakub
package days

import scala.annotation.tailrec
import scala.collection.immutable

case class Node(name: String, left: String, right: String)

object Day8 extends AdventOfCode {
  val fileNamePart1: String = "day8_part1.txt"
  val fileNamePart2: String = fileNamePart1

  def part1(input: List[String]): String = {
    val isEqualEndNode: String => Boolean = _.equals("ZZZ")

    getNumberOfSteps("AAA", isEqualEndNode, getGraph(getNodes(input)), input.head).toString
  }

  def part2(input: List[String]): String = {
    val endsWithZ: String => Boolean = _.endsWith("Z")

    lcmOfList(
      getGraph(getNodes(input))
        .filter { (name, node) => name.endsWith("A") }
        .map(startingNode => getNumberOfSteps(startingNode._1, endsWithZ, getGraph(getNodes(input)), input.head))
        .map(_.toLong)
        .toList
    ).toString
  }

  @tailrec
  private def getNumberOfSteps(
      startNodeName: String,
      predicate: String => Boolean,
      graph: Map[String, Node],
      path: String,
      idx: Int = 0
  ): Int = {
    val currentNodeName = path.charAt(idx % path.length) match {
      case 'R' => graph(startNodeName).right
      case 'L' => graph(startNodeName).left
    }

    if (predicate(currentNodeName)) idx + 1
    else getNumberOfSteps(currentNodeName, predicate, graph, path, idx + 1)
  }

  @tailrec
  private def gcd(a: Long, b: Long): Long = if (b == 0) a else gcd(b, a % b)

  private def lcm(a: Long, b: Long): Long = (a * b).abs / gcd(a, b)

  private def lcmOfList(numbers: List[Long]): Long = numbers.foldLeft(1L)(lcm)

  private def getGraph(nodes: List[Node]): Map[String, Node] = nodes.map(node => node.name -> node).toMap

  private def getNodes(input: List[String]) = input.drop(2).map { case s"$a = ($b, $c)" => Node(a, b, c) }
}
