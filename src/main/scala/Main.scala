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
  runDay(Day12)
}

def runDay(day: AdventOfCode): Unit = {
  val part1 = day.part1(getLines(day.fileNamePart1))
  val part2 = day.part2(getLines(day.fileNamePart2))

  println(s"\n${day.getClass.getSimpleName.stripSuffix("$")}")
  println(s"part 1: ${part1}")
  println(s"part 2: ${part2}\n")
}

def getLines(fileName: String): List[String] = {
  Using.resource(Source.fromResource(fileName)) { source =>
    source.getLines().toList
  }
}
