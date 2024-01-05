package se.jakub

import days.*

import scala.io.Source
import scala.util.Using

@main
def main(): Unit = {
  //  runDay(Day1)
  //  runDay(Day2)
  //  runDay(Day3)
  //  runDay(Day4)
  //  runDay(Day5)
  //  runDay(Day6)
  //  runDay(Day7)
  //  runDay(Day8)
  //  runDay(Day9)
  //  runDay(Day10)
  //  runDay(Day11)
  //  runDay(Day12)
  //  runDay(Day13)
  //  runDay(Day14)
  //  runDay(Day15)
  //  runDay(Day16)
  //  runDay(Day17)
  //  runDay(Day18)
  //  runDay(Day19)
  //  runDay(Day20)
  //  runDay(Day21)
  //  runDay(Day22)
  runDay(Day23)
}

def runDay(day: AdventOfCode): Unit = {
  println(s"\n${day.getClass.getSimpleName.stripSuffix("$")}")

  printPart("part 1", () => day.part1(getLines(day.fileNamePart1)))
  printPart("part 2", () => day.part2(getLines(day.fileNamePart2)))
}

private def printPart(partName: String, partFunction: () => String): Unit = {
  val startTime = System.nanoTime()
  val answer = partFunction()
  val endTime = System.nanoTime()
  val timeTaken = ((endTime - startTime) / 1e6d).round

  println(s"$partName: $answer ($timeTaken ms)")
}

private def getLines(fileName: String): List[String] = {
  Using.resource(Source.fromResource(fileName)) { source =>
    source.getLines().toList
  }
}
