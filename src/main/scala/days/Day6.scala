package se.jakub
package days

object Day6 extends AdventOfCode {
  val fileNamePart1: String = "day6_part1.txt"
  val fileNamePart2: String = fileNamePart1
  private val numberPattern = """(\d+)""".r

  def part1(input: List[String]): String = {
    def extractNumbers(str: String) = numberPattern.findAllMatchIn(str).map(x => x.toString().toLong).toList

    val recordTimes = extractNumbers(input.head)
    val recordDistances = extractNumbers(input.last)

    recordTimes
      .zip(recordDistances)
      .map { (totalTime, recordDistance) =>
        (0L to totalTime).count { t => totalDistance(totalTime, t) > recordDistance }
      }
      .product
      .toString
  }

  def part2(input: List[String]): String = {
    def extractSingleNumber(str: String): Long = numberPattern.findAllMatchIn(str).mkString.toLong

    val recordTime = extractSingleNumber(input.head)
    val recordDistance = extractSingleNumber(input.last)

    (0L to recordTime).count { t =>
      totalDistance(recordTime, t) > recordDistance
    }.toString
  }

  private def totalDistance(totalTime: Long, givenTime: Long) = givenTime * (totalTime - givenTime)
}
