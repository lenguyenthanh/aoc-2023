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
    loop(List.empty, parse(input)).foldMap(_.last)

  def countP2(input: String): Int =
    loop(List.empty, parse(input)).foldLeft(0)(-_ - -_.head)

  @annotation.tailrec
  private def loop(acc: List[List[Int]], current: List[Int]): List[List[Int]] =
    if hasSameItems(current) then current +: acc
    else loop(current +: acc, delta(current))

  def delta(xs: List[Int]) = xs.tail.zip(xs).map(_ - _)

  val hasSameItems: List[Int] => Boolean = _.toSet.size == 1

  def parse: String => List[Int] = _.split(" +").filter(_.trim.nonEmpty).map(_.toInt).toList
