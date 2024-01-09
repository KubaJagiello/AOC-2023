package se.jakub
package days

import java.util
import scala.Long.MaxValue
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.collection.parallel.CollectionConverters.*

case class RangeMap(destinationStart: Long, destinationEnd: Long, sourceStart: Long, sourceEnd: Long)

case class SeedTuple(currentValue: Long, rangeIndex: Int, startingValue: Long)

object Day5 extends AdventOfCode {
  val fileNamePart1: String = "day5_part1.txt"
  val fileNamePart2: String = fileNamePart1
  private val numberPattern = """(\d+)""".r

  def part1(input: List[String]): String = {
    val seeds = numberPattern.findAllMatchIn(input.head).map(x => x.toString().toLong).toList
    val ranges = createRanges(input)
    val lowestLocation = collection.mutable.HashMap[Long, Long]()

    for (seed <- seeds) {
      val linkedList = new util.LinkedList[SeedTuple]()
      linkedList.addFirst(SeedTuple(seed, 0, seed))

      while (!linkedList.isEmpty) {
        val seedTuple = linkedList.pollFirst()

        if (seedTuple.rangeIndex != ranges.length) {
          var found = false;
          for (range <- ranges(seedTuple.rangeIndex)) {
            if (range.sourceStart <= seedTuple.currentValue && range.sourceEnd >= seedTuple.currentValue) {
              val diff = Math.abs(range.sourceStart - range.destinationStart)
              if (range.sourceStart > range.destinationStart) {
                linkedList.addFirst(
                  SeedTuple(seedTuple.currentValue - diff, seedTuple.rangeIndex + 1, seedTuple.startingValue)
                )
              } else {
                linkedList.addFirst(
                  SeedTuple(seedTuple.currentValue + diff, seedTuple.rangeIndex + 1, seedTuple.startingValue)
                )
              }
              found = true
            }
          }

          if (!found) {
            linkedList.addFirst(SeedTuple(seedTuple.currentValue, seedTuple.rangeIndex + 1, seedTuple.startingValue))
          }
        } else {
          if (!lowestLocation.contains(seedTuple.startingValue)) {
            lowestLocation(seedTuple.startingValue) = MaxValue
          }
          val currentLowest = lowestLocation(seedTuple.startingValue)
          lowestLocation(seedTuple.startingValue) = Math.min(seedTuple.currentValue, currentLowest)
        }
      }
    }
    lowestLocation.values.min.toString
  }

  def part2(input: List[String]): String = {
    val validSeedValues = createSeedValues(input)
    val ranges = createRanges(input).reverse
    val lowestLocation = collection.mutable.HashMap[(Long, Long), Long]()
    var currentLowestFound = Long.MaxValue

    for (validSeed <- validSeedValues) {
      var location: Long = 0;
      var shouldBreak = false

      while (!solve(ranges, lowestLocation, location, validSeed, currentLowestFound) && !shouldBreak) {
        if (location > currentLowestFound) {
          shouldBreak = true
        }
        location += 1
      }
      currentLowestFound = lowestLocation.values.min
    }

    lowestLocation.values.min.toString
  }

  private def createSeedValues(input: List[String]) = {
    numberPattern
      .findAllMatchIn(input.head)
      .map(x => x.toString().toLong)
      .toList
      .grouped(2)
      .collect { case List(a, b) =>
        (a, a + b)
      }
      .toList
  }

  private def solve(
      ranges: List[ListBuffer[RangeMap]],
      lowestLocation: mutable.HashMap[(Long, Long), Long],
      locationNumber: Long,
      validSeed: (Long, Long),
      currentLowestFound: Long
  ): Boolean = {
    val linkedList = new util.LinkedList[SeedTuple]()
    var answer = false

    linkedList.addFirst(SeedTuple(locationNumber, 0, locationNumber))
    while (!linkedList.isEmpty) {
      val seedTuple = linkedList.pollFirst()

      if (seedTuple.rangeIndex != ranges.length) {
        var found = false;
        for (range <- ranges(seedTuple.rangeIndex)) {
          if (range.destinationStart <= seedTuple.currentValue && range.destinationEnd >= seedTuple.currentValue) {
            val diff = Math.abs(range.destinationStart - range.sourceStart)

            if (range.sourceStart < range.destinationStart) {
              linkedList.addFirst(
                SeedTuple(seedTuple.currentValue - diff, seedTuple.rangeIndex + 1, seedTuple.startingValue)
              )
            } else {
              linkedList.addFirst(
                SeedTuple(seedTuple.currentValue + diff, seedTuple.rangeIndex + 1, seedTuple.startingValue)
              )
            }
            found = true
          }
        }

        if (!found) {
          linkedList.addFirst(SeedTuple(seedTuple.currentValue, seedTuple.rangeIndex + 1, seedTuple.startingValue))
        }
      } else {
        if (seedTuple.currentValue >= validSeed._1 && seedTuple.currentValue < validSeed._2) {
          answer = true
          linkedList.clear()

          if (!lowestLocation.contains(validSeed)) {
            lowestLocation(validSeed) = MaxValue
          }
          val currentLowest = lowestLocation(validSeed)
          lowestLocation(validSeed) = Math.min(seedTuple.startingValue, currentLowest)
        }
      }
    }
    answer
  }

  private def createRanges(input: List[String]): List[ListBuffer[RangeMap]] = {
    val ranges = new ListBuffer[ListBuffer[RangeMap]]()
    var idx = 0;
    for (line <- input.drop(3)) {

      if (line.isEmpty) {
        idx += 1
      } else if (!line.contains("map")) {
        if (ranges.length <= idx) {
          ranges += new ListBuffer[RangeMap]()
        }

        val List(destination, source, range) = numberPattern.findAllMatchIn(line).map(x => x.toString().toLong).toList
        ranges(idx) += RangeMap(destination, destination + range, source, source + range - 1)
      }
    }
    ranges.toList
  }
}
