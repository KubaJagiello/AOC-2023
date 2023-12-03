package se.jakub

trait AdventOfCode {
  val fileNamePart1: String
  val fileNamePart2: String

  def part1(input: List[String]): String
  def part2(input: List[String]): String
}
