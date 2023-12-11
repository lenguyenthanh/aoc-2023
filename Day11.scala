package aoc

import cats.effect.*
import cats.syntax.all.*
import fs2.Stream

object Day11 extends AOCApp(2023, 11):

  def part1(input: Stream[IO, String]) = solve(input, _.solve(2))

  def part2(input: Stream[IO, String]) = solve(input, _.solve(1000000))

  def solve(input: Stream[IO, String], f: Solution => Any): IO[String] =
    input.map(Parser.parse).map(f).map(_.toString).compile.lastOrError

  case class Point(x: Long, y: Long):
    def distance(other: Point): Long =
      Math.abs(x - other.x) + Math.abs(y - other.y)

  case class Solution(points: List[Point]):
    val xs           = points.map(_.x).toSet
    val ys           = points.map(_.y).toSet
    val width        = xs.max + 1
    val height       = ys.max + 1
    val emptyColumns = List.range(0L, width).toSet -- xs
    val emptyRows    = List.range(0L, height).toSet -- ys

    def solve(times: Long): Long =
      val px = points.map: p =>
        val column = emptyColumns.count(_ < p.x)
        val row    = emptyRows.count(_ < p.y)
        Point(p.x + (times - 1) * column, p.y + (times - 1) * row)
      val t = for
        p1 <- px
        p2 <- px
        if p1 != p2
      yield p1.distance(p2)
      t.sum[Long] / 2

  object Parser:
    import cats.parse.Parser as P
    import cats.parse.Rfc5234.lf
    lazy val ignore = P.char('.').backtrack | lf
    lazy val galaxy = P.char('#')
    lazy val solution = (ignore.rep0.with1 *> (P.caret.with1 <* galaxy) <* ignore.rep0)
      .map(caret => Point(caret.col.toLong, caret.line.toLong))
      .rep0
      .map(Solution(_))
    def parse(input: String) = solution.parseAll(input).toOption.get
