package aoc

import cats.effect.*
import cats.syntax.all.*
import fs2.Stream

object Day10 extends AOCApp(2023, 10):

  def part1(input: Stream[IO, String]): IO[String] =
    sovle(input, _.solveP1())

  def part2(input: Stream[IO, String]): IO[String] =
    sovle(input, _.solveP2())

  def sovle(input: Stream[IO, String], f: Solution => Any): IO[String] =
    input
      .map(Parser.parse)
      .map(f)
      .map(_.toString)
      .compile
      .lastOrError

  enum Direction:
    case VP, HP, NE, NW, SE, SW

  case class Point(x: Int, y: Int)

  case class Solution(start: Point, map: Map[Point, Direction]):
    lazy val possibilities: List[Map[Point, Direction]] =
      Direction.values.toList.map(map.updated(start, _))

    def solveP1(): Int =
      possibilities.collectFirstSome(findPath).get.size / 2

    def solveP2(): Int =
      val (map, ls) = possibilities.collectFirstSome(x => findPath(x).map(x -> _)).get
      val path      = map.filter(x => ls.contains(x._1))
      val size      = 140
      val all = for
        x <- List.range(0, size)
        y <- List.range(0, size)
      yield Point(x, y)
      all.count(isInside(path, size))

    def isInside(path: Map[Point, Direction], size: Int)(p: Point): Boolean =
      if path.contains(p) then false
      else
        val n = List
          .range(p.x + 1, size)
          .foldLeft(0 -> none[Direction]): (pr, xd) =>
            val (acc, begin) = pr
            path.get(Point(xd, p.y)) match
              case None => acc -> none
              case Some(d) =>
                if d == Direction.NE || d == Direction.SE then acc -> d.some
                else if d == Direction.HP then acc + begin.fold(1)(_ => 0) -> begin
                else if d == Direction.NW || d == Direction.SW then
                  begin match
                    case None => acc -> begin
                    case Some(x) =>
                      if x == Direction.NE && d == Direction.NW then acc -> none
                      else if x == Direction.NE && d == Direction.SW then acc + 1 -> none
                      else if x == Direction.SE && d == Direction.SW then acc -> none
                      else if x == Direction.SE && d == Direction.NW then acc + 1 -> none
                      else acc                                                    -> begin
                else acc + 1 -> none
        n._1 % 2 == 1

    def findPath(m: Map[Point, Direction]) =
      val next = connectedTo(start, m(start)).head
      loop(m, next, start, Set(start, next))

    @annotation.tailrec
    private def loop(map: Map[Point, Direction], next: Point, current: Point, acc: Set[Point]): Option[Set[Point]] =
      map.get(next) match
        case None => None
        case Some(x) =>
          connectedTo(next, x).find(_ != current) match
            case None => None
            case Some(n) if n == start =>
              if connectedTo(start, map(start)).contains(next) then acc.some
              else none
            case Some(n) if acc.contains(n) => None
            case Some(n)                    => loop(map, n, next, acc + n)

    def connectedTo(point: Point, direction: Direction): List[Point] =
      direction match
        case Direction.VP => List(Point(point.x, point.y + 1), Point(point.x, point.y - 1))
        case Direction.HP => List(Point(point.x + 1, point.y), Point(point.x - 1, point.y))
        case Direction.NE => List(Point(point.x, point.y - 1), Point(point.x + 1, point.y))
        case Direction.NW => List(Point(point.x, point.y - 1), Point(point.x - 1, point.y))
        case Direction.SE => List(Point(point.x + 1, point.y), Point(point.x, point.y + 1))
        case Direction.SW => List(Point(point.x - 1, point.y), Point(point.x, point.y + 1))

  object Parser:
    import cats.parse.Parser as P
    import cats.parse.Rfc5234.lf

    lazy val ignore           = P.char('.').backtrack | lf
    lazy val direction        = P.charIn('|', '-', 'L', 'J', 'F', '7').map(charToDirection)
    lazy val startOrDirection = P.char('S').backtrack.as(().asLeft) | direction.map(_.asRight)
    lazy val tile = (ignore.rep0.with1 *> (P.caret.with1 ~ startOrDirection) <* ignore.rep0).map: (caret, value) =>
      Point(caret.col, caret.line) -> value

    lazy val tiles = tile.rep0.map: tiles =>
      val start = tiles.collectFirst { case (p, Left(_)) => p }.get
      val map   = tiles.collect { case (p, Right(d)) => p -> d }.toMap
      Solution(start, map)

    lazy val charToDirection: Char => Direction = _ match
      case '|' => Direction.VP
      case '-' => Direction.HP
      case 'L' => Direction.NE
      case 'J' => Direction.NW
      case 'F' => Direction.SE
      case '7' => Direction.SW

    def parse(str: String) = tiles.parseAll(str).toOption.get
