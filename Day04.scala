package aoc

import cats.effect.*
import cats.parse.Numbers.digits
import cats.parse.Parser as P
import cats.parse.Rfc5234.crlf
import fs2.Stream

object Day04 extends AOCApp(2023, 4):

  def part1(input: Stream[IO, String]): IO[String] =
    input
      .through(fs2.text.lines)
      .filter(_.trim.nonEmpty)
      .map(countP1)
      .fold(0)(_ + _)
      .compile
      .lastOrError
      .map(_.toString)

  def part2(input: Stream[IO, String]): IO[String] =
    input
      .through(fs2.text.lines)
      .filter(_.trim.nonEmpty)
      .map(countP2)
      .fold(Result.empty)((acc, card) => acc.add(card._1, card._2))
      .map(_.result)
      .compile
      .lastOrError
      .map(_.toString)

  val countP1: String => Int = _ match
    case s"Card $i: $left | $right" =>
      val winning = toSet(left.trim)
      val have    = toSet(right.trim)
      java.lang.Math.pow(2, winning.intersect(have).size - 1).toInt

  val countP2: String => (Int, Int) = _ match
    case s"Card $i: $left | $right" =>
      val winning = toSet(left.trim)
      val have    = toSet(right.trim)
      i.trim.toInt -> winning.intersect(have).size

  case class Result(result: Int, bonus: Map[Int, Int]):
    def add(cardIdx: Int, count: Int): Result =
      val copies = instances(cardIdx)
      val newSum = copies + result
      val newBonus = List
        .range(cardIdx + 1, cardIdx + count + 1)
        .foldLeft(bonus):
          case (acc, idx) => acc.updated(idx, bonus.getOrElse(idx, 0) + copies)
      Result(newSum, newBonus)

    def instances(cardIdx: Int): Int =
      bonus.getOrElse(cardIdx, 0) + 1

  object Result:
    val empty = Result(0, Map.empty)

  def toSet: String => Set[Int] = _.split(" +").filter(_.trim.nonEmpty).map(_.toInt).toSet
