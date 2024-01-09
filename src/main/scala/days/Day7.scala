package se.jakub
package days

object Day7 extends AdventOfCode {
  val fileNamePart1: String = "day7_part1.txt"
  val fileNamePart2: String = fileNamePart1

  def part1(input: List[String]): String = {
    input
      .map { line => line.split(" ") }
      .map { case Array(cards, bid) => ((cards, bid.toLong)) }
      .sortWith(sortPart1)
      .reverse
      .zipWithIndex
      .foldLeft(0L) { case (acc, ((cards, bid), idx)) =>
        acc + ((idx + 1).toLong * bid)
      }
      .toString
  }

  private def sortPart1(a: (String, Long), b: (String, Long)): Boolean = {
    compareCards(
      a,
      b,
      List("A", "K", "Q", "J", "T", "9", "8", "7", "6", "5", "4", "3", "2").zipWithIndex.toMap,
      cardsType(getCharacterCounts(a).values.toList),
      cardsType(getCharacterCounts(b).values.toList)
    )
  }

  def part2(input: List[String]): String = {
    input
      .map { line => line.split(" ") }
      .map { case Array(cards, bid) => ((cards, bid.toLong)) }
      .sortWith(sortPart2)
      .reverse
      .zipWithIndex
      .foldLeft(0L) { case (acc, ((cards, bid), idx)) =>
        acc + ((idx + 1).toLong * bid)
      }
      .toString
  }

  private def sortPart2(a: (String, Long), b: (String, Long)): Boolean = {
    compareCards(
      a,
      b,
      List("A", "K", "Q", "T", "9", "8", "7", "6", "5", "4", "3", "2", "J").zipWithIndex.toMap,
      cardsType(getCardsValues(getCharacterCounts(a))),
      cardsType(getCardsValues(getCharacterCounts(b)))
    )
  }

  private def compareCards(
      a: (String, Long),
      b: (String, Long),
      cardsStrengths: Map[String, Int],
      aCardsTypeValue: Long,
      bCardsTypeValue: Long
  ): Boolean = {
    if (aCardsTypeValue != bCardsTypeValue) aCardsTypeValue > bCardsTypeValue
    else cardsStrength(a._1, b._1, cardsStrengths)
  }

  private def cardsStrength(a: String, b: String, cardsValues: Map[String, Int]): Boolean = {
    a.indices.foreach { i =>
      val strengthA = cardsValues(a(i).toString)
      val strengthB = cardsValues(b(i).toString)

      if (strengthA < strengthB) return true
      else if (strengthA > strengthB) return false
    }

    false
  }

  private def getCharacterCounts(a: (String, Long)) = {
    a._1.groupBy(identity).view.mapValues(_.length).toMap
  }

  private def cardsType(values: List[Int]): Long = {
    if values.contains(5) then return 7
    else if values.contains(4) then return 6
    else if values.contains(3) && values.contains(2) then return 5
    else if values.contains(3) then return 4
    else if values.count { v => v == 2 } == 2 then return 3
    else if values.count { v => v == 2 } == 1 then return 2
    1
  }

  private def getCardsValues(cardsCount: Map[Char, Int]): List[Int] = {
    val valuesWithoutJ = cardsCount.filter(_._1 != 'J').values.toList.sorted.reverse

    if (cardsCount.get('J').exists(_ < 5)) {
      val updatedValue = valuesWithoutJ.headOption.getOrElse(0) + cardsCount('J')
      updatedValue :: valuesWithoutJ.drop(1)
    } else {
      cardsCount.values.toList
    }
  }
}
