package se.jakub
package days

import scala.collection.mutable

object Day22 extends AdventOfCode {
  val fileNamePart1: String = "day22_part1.txt"
  val fileNamePart2: String = fileNamePart1

  private class Point3D(var x: Int, var y: Int, var z: Int)

  private class Cube(a: Array[Int], b: Array[Int]) {
    val f: Point3D = Point3D(a(0), a(1), a(2))
    val s: Point3D = Point3D(b(0), b(1), b(2))
  }

  def part1(input: List[String]): String = {
    val cubes = simulateFallingCubes(input.map(getCords).sortBy(_.f.z))
    val (aSupportsB, bIsSupportedByA) = initializeAndPopulateSupportMaps(cubes)

    cubes.indices.count { i =>
      aSupportsB(i).map(bIsSupportedByA).forall(_.size >= 2)
    }.toString
  }

  def part2(input: List[String]): String = {
    val cubes = simulateFallingCubes(input.map(getCords).sortBy(_.f.z))
    val (aSupportsB, bIsSupportedByA) = initializeAndPopulateSupportMaps(cubes)

    cubes.indices.foldLeft(0) { (acc, i) =>
      val fallingCubes = new mutable.HashSet[Int]().addAll(getCubesSupportedByOneCube(aSupportsB, bIsSupportedByA, i)) + i
      val q = new mutable.Queue[Int]().enqueueAll(fallingCubes)

      while (q.nonEmpty) {
        val j = q.dequeue()
        aSupportsB(j).diff(fallingCubes).foreach { k =>
          if (bIsSupportedByA(k).subsetOf(fallingCubes)) {
            q.enqueue(k)
            fallingCubes.add(k)
          }
        }
      }
      acc + fallingCubes.size - 1
    }.toString
  }

  private def getCubesSupportedByOneCube(aSupportsB: Map[Int, Set[Int]], bIsSupportedByA: Map[Int, Set[Int]], i: Int) = {
    aSupportsB(i).map(bIndex => (bIndex, bIsSupportedByA(bIndex))).filter(_._2.size == 1).map(x => x._1)
  }

  private def simulateFallingCubes(cubes: List[Cube]): List[Cube] = {
    cubes.zipWithIndex.foreach { case (fallingCube, i) =>
      val lowestZ = (0 until i)
        .collect { case j if xAndYOverlaps(fallingCube, cubes(j)) => cubes(j).s.z + 1 }
        .foldLeft(1)(math.max)

      fallingCube.s.z -= fallingCube.f.z - lowestZ
      fallingCube.f.z = lowestZ
    }

    cubes.sortBy(_.f.z)
  }

  private def cubesTouch(a: Cube, b: Cube): Boolean = xAndYOverlaps(a, b) && b.f.z == a.s.z + 1

  private def xAndYOverlaps(a: Cube, b: Cube): Boolean =
    (a.f.x max b.f.x) <= (a.s.x min b.s.x) && (a.f.y max b.f.y) <= (a.s.y min b.s.y)

  private def getCords(line: String): Cube = {
    val Array(firstPoint, secondPoint) = line.split("~").map(_.split(",").map(_.toInt))
    Cube(firstPoint, secondPoint)
  }

  private def initializeAndPopulateSupportMaps(cubes: List[Cube]): (Map[Int, Set[Int]], Map[Int, Set[Int]]) = {
    val aSupportsB = new mutable.HashMap[Int, mutable.HashSet[Int]]()
    val bSupportedByA = new mutable.HashMap[Int, mutable.HashSet[Int]]()

    cubes.indices.foreach { i =>
      aSupportsB(i) = new mutable.HashSet[Int]()
      bSupportedByA(i) = new mutable.HashSet[Int]()
    }

    cubes.zipWithIndex.foreach { case (upper, j) =>
      (0 until j).foreach { i =>
        val lower = cubes(i)
        if (cubesTouch(lower, upper)) {
          aSupportsB(i) += j
          bSupportedByA(j) += i
        }
      }
    }

    (aSupportsB.view.mapValues(_.toSet).toMap, bSupportedByA.view.mapValues(_.toSet).toMap)
  }
}
