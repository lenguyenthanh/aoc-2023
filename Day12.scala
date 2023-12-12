package aoc

import cats.effect.*
import cats.syntax.all.*
import fs2.Stream

object Day12 extends AOCApp(2023, 12):

  def part1(input: Stream[IO, String]): IO[String] =
    input.run(_.foldMap(parse(_).solveP1))

  def part2(input: Stream[IO, String]): IO[String] =
    input.run(_.foldMap(parse(_).solveP2))

  enum Spring:
    case Good, Bad, Unknown

  case class Solution(spring: List[Spring], config: List[Int]):
    def solveP1: Int =
      val all = spring.foldLeft[List[List[Spring]]](Nil): (acc, x) =>
        val xs = x match
          case Spring.Unknown => List(Spring.Good, Spring.Bad)
          case _              => List(x)
        acc match
          case Nil =>
            x match
              case Spring.Unknown => List(List(Spring.Good), List(Spring.Bad))
              case _              => List(List(x))
          case _ =>
            for
              a <- acc
              x <- xs
            yield x +: a
      all.count(x => findConfig(x.reverse) == config)

    def solveP2: Long =
      val ns    = List.fill(5)(spring).foldLeft(List.empty[Spring])((acc, x) => acc ++ x ++ List(Spring.Unknown)).init
      val nc    = List.fill(5)(config).flatten
      val cache = scala.collection.mutable.Map.empty[(List[Spring], List[Int], Int), Long]
      def loop(xs: List[Spring], cs: List[Int], count: Int): Long =
        if cache.contains((xs, cs, count)) then cache((xs, cs, count))
        else
          val r = xs match
            case Nil =>
              if (count == 0 && cs.isEmpty) || cs == List(count) then 1L
              else 0L
            case x :: tail =>
              x match
                case Spring.Unknown =>
                  loop(Spring.Bad +: tail, cs, count) + loop(Spring.Good +: tail, cs, count)
                case Spring.Bad =>
                  if cs.headOption.contains(count) then 0L
                  else loop(tail, cs, count + 1)
                case Spring.Good =>
                  if cs.headOption.contains(count) then loop(tail, cs.tail, 0)
                  else if count == 0 then loop(tail, cs, 0)
                  else 0L
          cache((xs, cs, count)) = r
          r
      loop(ns, nc, 0)

  def findConfig(xs: List[Spring]): List[Int] =
    val r = xs.foldLeft[(Int, List[Int])](0 -> Nil): (acc, x) =>
      val (current, ys) = acc
      x match
        case Spring.Bad => (current + 1, ys)
        case Spring.Good =>
          if current > 0 then (0, current +: ys)
          else (0, ys)
        case _ => acc
    if r._1 > 0 then (r._1 +: r._2).reverse
    else r._2.reverse

  def parse(input: String) = input match
    case s"$spring $config" => Solution(parseSpring(spring), parseConfig(config))

  val parseSpring: String => List[Spring] = _.trim.toList.map(_ match
    case '.' => Spring.Good
    case '#' => Spring.Bad
    case '?' => Spring.Unknown
  )

  val parseConfig: String => List[Int] = _.trim.split(",").toList.map(_.toInt)
