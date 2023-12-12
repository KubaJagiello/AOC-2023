package se.jakub
package days

import java.util
import scala.collection.mutable

object Day12 extends AdventOfCode {
  val fileNamePart1: String = "day12_part1.txt"
  val fileNamePart2: String = fileNamePart1

  type Group = List[Int]

  def part1(input: List[String]): String = {
    var answer = 0
    for (elem <- input) {
      val springs = elem.split(" ")(0)
      val numbers = parseNumbers(elem.split(" ")(1))
      answer += noOfArrangements(springs, numbers)
    }
    answer.toString
  }

  def part2(input: List[String]): String = {
    var answer = 0
    for ((elem, idx) <- input.zipWithIndex) {
      val springs = elem.split(" ")(0)
      val repeatedSprings = List.fill(5)(springs).mkString("?")
      val numbers = parseNumbers(elem.split(" ")(1))
      val repeatedList = List.fill(5)(numbers).flatten
      println(idx)
      answer += noOfArrangements2(repeatedSprings, repeatedList)
    }
    answer.toString
  }

  private def noOfArrangements(springs: String, group: Group): Int = {
    val queue = mutable.Queue[String]()
    queue.enqueue(springs)
    var count = 0

    while (queue.nonEmpty) {
      val current = queue.dequeue()
      if (current.contains('?')) {
        queue.enqueue(current.replaceFirst("\\?", "."))
        queue.enqueue(current.replaceFirst("\\?", "#"))
      } else {
        if (isValidArrangement(current, group)) {
          count += 1
        }
      }
    }

    count
  }

  private def noOfArrangements2(springs: String, group: Group): Int = {
    val queue = util.LinkedList[String]()
    queue.addFirst(springs)
    var count = 0

    while (!queue.isEmpty) {
      val current = queue.pollFirst()

      if (isValidPartialArrangement(current, group)) {
        if (current.contains('?')) {
          queue.addFirst(current.replaceFirst("\\?", "."))
          queue.addFirst(current.replaceFirst("\\?", "#"))
        } else {
          if (isValidArrangement(current, group)) {
            count += 1
          }
        }
      }
    }

    count
  }

  private def isValidPartialArrangement(record: String, group: Group): Boolean = {
    val splitRecord = record.split("\\.").filter(!_.isBlank).takeWhile(!_.contains('?'))

    if(splitRecord.isEmpty){
      return true
    }
    if(splitRecord.length > group.length) {
      return false
    }

    splitRecord.indices
      .map(i => (splitRecord(i), group(i)))
      .forall((subRecord, subGroup) => subRecord.count(_.equals('#')) == subGroup)
  }


  private def isValidArrangement(record: String, group: Group): Boolean = {
    val splitRecord = record.split("\\.").filter(!_.isBlank)
    if (splitRecord.length != group.length) {
      return false
    }

    splitRecord.indices
      .map(i => (splitRecord(i), group(i)))
      .forall((subRecord, subGroup) => subRecord.count(_.equals('#')) == subGroup)
  }

  private def parseNumbers(line: String): Group = {
    line.split(",").map(_.toInt).toList
  }

}
