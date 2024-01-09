package se.jakub
package days

object Day24 extends AdventOfCode {
  val fileNamePart1: String = "day24_part1.txt"
  val fileNamePart2: String = fileNamePart1
  private val minValue = 200000000000000L
  private val maxValue = 400000000000000L

  def part1(input: List[String]): String = {
    val vectors = parseXYVectors(input)
    var answer = 0

    for (i <- vectors.indices) {
      for (v2 <- vectors.drop(i + 1)) {
        val v1 = vectors(i)

        v1.intersect(v2) match {
          case Some(x, y) =>
            if (isValid(v2, v1, x, y)) {
              answer += 1
            }
          case _ =>
        }
      }
    }

    answer.toString
  }

  private def parseXYVectors(input: List[String]): List[Vector2D] = {
    input.map(parseInput).map { case Array(x, y, _, vx, vy, _) =>
      Vector2D(x, y, vx, vy)
    }
  }

  private def parseInput(line: String): Array[Long] = {
    line.replace(" @ ", ", ").replaceAll(" ", "").split(",").map(_.toLong)
  }

  private def isValid(
      v2: Vector2D,
      v1: Vector2D,
      x: BigDecimal,
      y: BigDecimal
  ) = {
    x <= maxValue && x >= minValue && y <= maxValue && y >= minValue && v1.time(
      x,
      y
    ) >= 0 && v2.time(x, y) >= 0
  }

  def part2(input: List[String]): String = {
    val xyVectors = parseXYVectors(input)
    val xzVectors = parseXZVectors(input)

    val (x, y) = bruteForceIntersection(xyVectors)
    val (_, z) = bruteForceIntersection(xzVectors)

    (x + y + z).toString
  }

  private def bruteForceIntersection(vectors: List[Vector2D]): (Long, Long) = {
    (-300 to 300).view
      .flatMap(i => (-300 to 300).flatMap(j => findOrigin(vectors, i, j)))
      .head
  }

  private def findOrigin(
      vectors: List[Vector2D],
      vx: Long,
      vy: Long
  ): Option[(Long, Long)] =
    val Array(v1, v2, v3) =
      vectors.map(_.offsetVelocity(vx, vy)).toArray.take(3)
    for {
      (x1, y1) <- v1.intersect(v2)
      (x2, y2) <- v1.intersect(v3)
      if x1 == x2 && y1 == y2
      time = v1.time(x1, y1)

    } yield (v1.x + v1.vx * time.longValue, v1.y + v1.vy * time.longValue)

  private def parseXZVectors(input: List[String]): List[Vector2D] = {
    input.map(parseInput).map { case Array(x, _, z, vx, _, vz) =>
      Vector2D(x, z, vx, vz)
    }
  }

  private case class Vector2D(x: Long, y: Long, vx: Long, vy: Long) {
    private val a: BigDecimal = BigDecimal(vy)
    private val b: BigDecimal = BigDecimal(-vx)
    private val c: BigDecimal = BigDecimal(vx * y - vy * x)

    def intersect(v: Vector2D): Option[(BigDecimal, BigDecimal)] = {
      val d = a * v.b - v.a * b
      Option.when(d != 0) { ((b * v.c - v.b * c) / d, (c * v.a - v.c * a) / d) }
    }

    def offsetVelocity(dvx: Long, dvy: Long): Vector2D =
      Vector2D(x, y, vx - dvx, vy - dvy)

    def time(posX: BigDecimal, posY: BigDecimal): BigDecimal =
      (posY - y) / vy max (posX - x) / vx
  }
}
