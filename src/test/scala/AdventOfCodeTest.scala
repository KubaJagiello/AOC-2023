package se.jakub

import days.*

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks.*
import org.scalatest.prop.TableFor3

class AdventOfCodeTest extends AnyFunSuite {
  val testCases: TableFor3[AdventOfCode, String, String] = Table(
    ("day", "part1Result", "part2Result"),
    //    (Day1, "52974", "53340"),
    //    (Day2, "2913", "55593"),
    //    (Day3, "539433", "75847567"),
    //    (Day4, "23678", "15455663"),
    //    (Day5, "424490994", "15290096") // takes currently too long
    //    (Day6, "170000", "20537782"),
    //    (Day7, "250951660", "251481660"),
    //    (Day8, "19637", "8811050362409"),
    //    (Day9, "1974913025", "884"),
    //    (Day10, "7145", "445"),
    //    (Day11, "10292708", "790194712336"),
    //    (Day12, "7017", "527570479489"),
    //    (Day13, "34993", "29341"),
    //    (Day14, "108935", "100876"),
    //    (Day15, "504036", "295719"),
    //    (Day16, "7517", "7741"),
    //    (Day17, "1099", "1266"),
    //    (Day18, "52035", "60612092439765"),
    //    (Day19, "399284", "121964982771486")
    (Day20, "886347020", "233283622908263")
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
