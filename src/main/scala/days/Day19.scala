package se.jakub
package days

import scala.collection.mutable

object Day19 extends AdventOfCode {
  val fileNamePart1: String = "day19_part1.txt"
  val fileNamePart2: String = fileNamePart1
  private val ratingRegex = """([a-z]=\d+)""".r
  private val workflowRegex = """([a-z][<>]\d+:\w+)|(\b\w+\b)""".r
  private val boundaryRegex = """([a-zA-Z]+)([<>])(\d+):([a-zA-Z]+)""".r

  def part1(input: List[String]): String = {
    val workflows = parseWorkflow(input).map(workflow => workflow.name -> workflow).toMap
    val ratings = input.reverse.takeWhile(_.nonEmpty).map(parseRatings)

    ratings.reverse
      .map(rating => if (solvePart1(rating, "in", workflows)) rating.values.sum else 0)
      .sum
      .toString
  }

  def part2(input: List[String]): String = {
    val workflows = parseWorkflow(input).map(workflow => workflow.name -> workflow).toMap
    val ranges = Map("x" -> (1, 4000), "m" -> (1, 4000), "a" -> (1, 4000), "s" -> (1, 4000))

    solvePart2(ranges, workflows).toString
  }

  private def solvePart2(ranges: Map[String, (Int, Int)], workFlowMap: Map[String, WorkFlow]): Long = {
    val q = mutable.Queue[(String, Map[String, (Int, Int)])](("in", ranges))
    var ans = 0L

    while (q.nonEmpty) {
      val (workflowName, ranges) = q.dequeue()

      if (workflowName == "A") {
        ans += ranges.values.map((mn, mx) => mx - mn + 1L).product
      } else if (workflowName != "R") {
        val currentWorkflow = workFlowMap(workflowName)
        val mutableRanges = new mutable.HashMap[String, (Int, Int)]().addAll(ranges)

        for (boundary <- currentWorkflow.boundaries) {
          val (minR, maxR) = mutableRanges(boundary.name)
          val newRanges = new mutable.HashMap[String, (Int, Int)]().addAll(mutableRanges)

          if (boundary.c == "<") {
            val (newMinR, newMaxR) = (minR, Math.min(maxR, boundary.value - 1))
            val (newMinR2, newMaxR2) = (Math.max(boundary.value, minR), maxR)

            if (newMinR2 <= newMaxR2) {
              mutableRanges(boundary.name) = (newMinR2, newMaxR2)
            }
            if (newMinR <= newMaxR) {
              newRanges(boundary.name) = (newMinR, newMaxR)
            }

          } else {
            val (newMinR, newMaxR) = (Math.max(minR, boundary.value + 1), maxR)
            val (newMinR2, newMaxR2) = (minR, Math.min(boundary.value, maxR))

            if (newMinR2 <= newMaxR2) {
              mutableRanges(boundary.name) = (newMinR2, newMaxR2)
            }
            if (newMinR <= newMaxR) {
              newRanges(boundary.name) = (newMinR, newMaxR)
            }
          }
          q.enqueue((boundary.nextWorkFlow, newRanges.toMap))
        }
        q.enqueue((currentWorkflow.fallbackWorkflow, mutableRanges.toMap))
      }
    }
    ans
  }

  private def parseWorkflow(lines: List[String]): List[WorkFlow] = {
    lines.takeWhile(_.nonEmpty).map { line =>
      val workflowString = workflowRegex.findAllIn(line).toList

      val boundaries = (1 until workflowString.size - 1)
        .flatMap(i =>
          boundaryRegex
            .findAllMatchIn(workflowString(i))
            .map(m => Boundary(m.group(1), m.group(2), m.group(3).toInt, m.group(4)))
            .toList
            .headOption
        )

      WorkFlow(workflowString.head, boundaries.toList, workflowString.last)
    }
  }

  private def solvePart1(
      rating: Map[String, Int],
      currentWorkFlowName: String,
      workFlowMap: Map[String, WorkFlow]
  ): Boolean = {
    currentWorkFlowName match {
      case "A" => true
      case "R" => false
      case _ =>
        workFlowMap(currentWorkFlowName).boundaries
          .find(boundary => satisfiesBoundary(rating(boundary.name), boundary.c, boundary.value))
          .map(boundary => solvePart1(rating, boundary.nextWorkFlow, workFlowMap))
          .getOrElse(solvePart1(rating, workFlowMap(currentWorkFlowName).fallbackWorkflow, workFlowMap))
    }
  }

  private def parseRatings(line: String): Map[String, Int] = {
    ratingRegex.findAllIn(line).toList.map(s => s.split("=")).map(s => s(0) -> s(1).toInt).toMap
  }

  private def satisfiesBoundary(a: Int, c: String, b: Int): Boolean = if c == ">" then a > b else a < b

  private case class Boundary(name: String, c: String, value: Int, nextWorkFlow: String)

  private case class WorkFlow(name: String, boundaries: List[Boundary], fallbackWorkflow: String)
}
