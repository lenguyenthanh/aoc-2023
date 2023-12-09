package aoc

import cats.effect.*
import cats.syntax.all.*
import fs2.Stream

object Day06 extends AOCApp(2023, 6):

  def part1(input: Stream[IO, String]): IO[String] =
    input
      .map(Parser.parse)
      .map(_.solveP1())
      .map(_.toString)
      .compile
      .lastOrError

  def part2(input: Stream[IO, String]): IO[String] =
    input
      .map(Parser.parse)
      .evalMap(_.solveP2())
      .map(_.toString)
      .compile
      .lastOrError

  case class Race(time: Long, distance: Long):

    def timeToRace(hold: Long): Int =
      val d = hold * (time - hold)
      if d >= distance then 1 else 0

    def ways: Int =
      Range.Long(1, time, 1).toList.foldMap(timeToRace)

    def waysIO: IO[Int] =
      Stream
        .range(1L, time)
        .covary[IO]
        .foldMap(timeToRace)
        .compile
        .lastOrError

  case class Solution(races: List[Race]):
    def solveP1(): Long =
      races.foldLeft(1):
        case (acc, r) => acc * r.ways

    def solveP2(): IO[Int] =
      Race(toBig(races.map(_.time)), toBig(races.map(_.distance))).waysIO

    def toBig(l: List[Long]): Long =
      l.mkString("").toLong

  object Parser:
    def parse(input: String): Solution =
      input.split("\n").toList match
        case times :: distances :: Nil =>
          Solution(parseTimes(times).zip(parseDistances(distances)).map(Race.apply.tupled))

    val parseTimes: String => List[Long] = _ match
      case s"Time: $xs" => xs.trim.split(" +").map(_.toLong).toList

    val parseDistances: String => List[Long] = _ match
      case s"Distance: $xs" => xs.trim.split(" +").map(_.toLong).toList
