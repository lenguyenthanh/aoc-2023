package aoc

import cats.effect.*
import cats.syntax.all.*
import fs2.Stream

object Day10 extends AOCApp(2023, 10):

  def part1(input: Stream[IO, String]) = solve(input, _.solveP1())

  def part2(input: Stream[IO, String]) = solve(input, _.solveP2())

  def solve(input: Stream[IO, String], f: Solution => Any): IO[String] =
    input.map(Parser.parse).map(f).map(_.toString).compile.lastOrError

  enum Direction(val value: Char):
    case VP extends Direction('|')
    case HP extends Direction('-')
    case NE extends Direction('L')
    case NW extends Direction('J')
    case SE extends Direction('F')
    case SW extends Direction('7')
  import Direction.*

  case class Point(x: Int, y: Int)

  type Grid = Map[Point, Direction]

  case class Solution(start: Point, map: Grid, size: Int):
    lazy val possibilities: List[Grid] =
      Direction.values.toList.map(map.updated(start, _))

    def solveP1(): Int =
      possibilities.collectFirstSome(findPath).get.size / 2

    def solveP2(): Int =
      val (map, ls) = possibilities.collectFirstSome(x => findPath(x).map(x -> _)).get
      val path      = map.filter(x => ls.contains(x._1))
      val all = for
        x <- List.range(0, size)
        y <- List.range(0, size)
      yield Point(x, y)
      all.count(isInside(path))

    def isInside(path: Grid)(p: Point): Boolean =
      if path.contains(p) then false
      else
        val n = List
          .range(p.x + 1, size)
          .foldLeft(0 -> none[Direction]): (pr, xd) =>
            val (acc, begin) = pr
            path.get(Point(xd, p.y)) match
              case None => acc -> none
              case Some(d) =>
                if d == NE || d == SE then acc -> d.some
                else if d == VP then acc + 1 -> none
                else if d == NW || d == SW then
                  begin match
                    case Some(x) if (x == SE && d == NW) || (x == NE && d == SW) =>
                      acc + 1 -> none
                    case _ => acc -> begin
                else acc -> begin
        n._1 % 2 == 1

    def findPath(m: Grid) =
      val next = connectedTo(start, m(start)).head
      loop(m, next, start, Set(start, next))

    @annotation.tailrec
    private def loop(map: Grid, current: Point, previous: Point, acc: Set[Point]): Option[Set[Point]] =
      map.get(current) match
        case None => None
        case Some(x) =>
          connectedTo(current, x).find(_ != previous) match
            case None => None
            case Some(next) if next == start =>
              if connectedTo(start, map(start)).contains(current) then acc.some
              else none
            case Some(n) if acc.contains(n) => none
            case Some(next)                 => loop(map, next, current, acc + next)

    def connectedTo(point: Point, direction: Direction): List[Point] =
      direction match
        case VP => List(Point(point.x, point.y + 1), Point(point.x, point.y - 1))
        case HP => List(Point(point.x + 1, point.y), Point(point.x - 1, point.y))
        case NE => List(Point(point.x, point.y - 1), Point(point.x + 1, point.y))
        case NW => List(Point(point.x, point.y - 1), Point(point.x - 1, point.y))
        case SE => List(Point(point.x + 1, point.y), Point(point.x, point.y + 1))
        case SW => List(Point(point.x - 1, point.y), Point(point.x, point.y + 1))

  object Parser:
    import cats.parse.Parser as P
    import cats.parse.Rfc5234.lf

    lazy val directionMap     = Direction.values.map(x => x.value -> x).toMap
    lazy val ignore           = P.char('.').backtrack | lf
    lazy val direction        = P.charIn(directionMap.keySet).map(directionMap(_))
    lazy val startOrDirection = P.char('S').backtrack.as(().asLeft) | direction.map(_.asRight)
    lazy val solution = (ignore.rep0.with1 *> (P.caret.with1 ~ startOrDirection) <* ignore.rep0)
      .map: (caret, value) =>
        Point(caret.col, caret.line) -> value
      .rep0
      .map: xs =>
        val start = xs.collectFirst { case (p, Left(_)) => p }.get
        val map   = xs.collect { case (p, Right(d)) => p -> d }.toMap
        start -> map

    def parse(str: String) =
      val (start, map) = solution.parseAll(str).toOption.get
      Solution(start, map, str.split("\n").toList.size)
