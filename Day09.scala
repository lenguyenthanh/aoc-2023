package aoc

import cats.effect.*
import cats.syntax.all.*
import fs2.Stream

object Day09 extends AOCApp(2023, 9):

  def part1(input: Stream[IO, String]): IO[String] =
    input.run(_.foldMap(countP1))

  def part2(input: Stream[IO, String]): IO[String] =
    input.run(_.foldMap(countP2))

  def countP1(input: String): Int =
    loop(List.empty, parse(input)).foldLeft(0):
      (acc, x) => acc + x.last

  def countP2(input: String): Int =
    loop(List.empty, parse(input)).foldLeft(0):
      (acc, x) => x.head - acc

  @annotation.tailrec
  private def loop(acc: List[List[Int]], current: List[Int]): List[List[Int]] =
    if current.isTheSame then current +: acc
    else loop(current +: acc, delta(current))

  def delta(xs: List[Int]): List[Int] =
    xs.zip(xs.tail).map((x, y) => y - x)

  extension (xs: List[Int])
    def isTheSame: Boolean = xs.toSet.size == 1

  def parse: String => List[Int] = _.split(" +").filter(_.trim.nonEmpty).map(_.toInt).toList
