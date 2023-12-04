package aoc

import cats.effect.*
import fs2.Stream

object Day04 extends AOCApp(2023, 4):

  def part1(input: Stream[IO, String]): IO[String] =
    val solve: fs2.Pipe[IO, String, Int] =
      _.map(countP1)
        .fold(0)(_ + _)
    input.run(solve)

  def part2(input: Stream[IO, String]): IO[String] =
    val solve: fs2.Pipe[IO, String, Int] =
      _.map(countP2)
        .fold(Result.empty)(_ add _)
        .map(_.result)
    input.run(solve)

  def countP1(input: String): Int =
    val (_, winning, have) = parse(input)
    java.lang.Math.pow(2, winning.intersect(have).size - 1).toInt

  def countP2(input: String): (Int, Int) =
    val (i, winning, have) = parse(input)
    i -> winning.intersect(have).size

  def parse: String => (Int, Set[Int], Set[Int]) = _ match
    case s"Card $i: $left | $right" =>
      val winning = toSet(left.trim)
      val have    = toSet(right.trim)
      (i.trim.toInt, winning, have)

  case class Result(result: Int, bonus: Map[Int, Int]):
    infix def add(p: Tuple2[Int, Int]): Result =
      val (cardIdx, count) = p
      val copies           = instances(cardIdx)
      val newSum           = copies + result
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
