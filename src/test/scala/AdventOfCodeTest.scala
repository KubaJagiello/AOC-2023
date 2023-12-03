package se.jakub

import days.*

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks.*
import org.scalatest.prop.TableFor3

class AdventOfCodeTest extends AnyFunSuite {
  val testCases: TableFor3[AdventOfCode, String, String] = Table(
    ("day", "part1Result", "part2Result"),
    (Day1, "52974", "53340"),
    (Day2, "2913", "55593"),
    (Day3, "539433", "75847567")
  )

  forAll(testCases) { (day, part1Result, part2Result) =>
    test(s"${day.getClass.getSimpleName} part1 returns expected result") {
      assert(day.part1(getLines(day.fileNamePart1)) == part1Result)
    }

    test(s"${day.getClass.getSimpleName} part2 returns expected result") {
      assert(day.part2(getLines(day.fileNamePart2)) == part2Result)
    }
  }
}
