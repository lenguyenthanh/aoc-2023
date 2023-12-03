package aoc

import cats.effect.*
import cats.parse.Numbers.digits
import cats.parse.Parser as P
import fs2.Stream

object Day03 extends AOCApp(2023, 3):

  def part1(input: Stream[IO, String]): IO[String] =
    run(input, _.calculateP1())

  def part2(input: Stream[IO, String]): IO[String] =
    run(input, _.calculateP2())

  def run(input: Stream[IO, String], f: Schematic => Long): IO[String] =
    input.zipWithIndex
      .map((x, y) => parse(x, y.toInt))
      .fold(Schematic.empty): (schematic, engines) =>
        engines.foldLeft(schematic)(_ add _)
      .map(f)
      .map(_.toString)
      .compile
      .lastOrError

  enum Engine:
    case Number(point: Point, length: Int, value: Long)
    case Symbol(point: Point, length: Int, value: Char)

  extension (n: Engine.Number) def adjacent = n.point.adjacent(n.length)

  case class Point(x: Int, y: Int):
    def adjacent(length: Int): List[Point] =
      for
        x1 <- List.range(x - 1, x + length + 1)
        y1 <- List.range(y - 1, y + 2)
        if !(y1 == y && x <= x1 && x1 <= x + length - 1) && x >= 0 && y >= 0
      yield Point(x1, y1)

  case class Schematic(numbers: List[Engine.Number], symbols: Map[Point, Engine.Symbol]):
    infix def add(engine: Engine): Schematic =
      engine match
        case n @ Engine.Number(_, _, _) =>
          copy(numbers = n +: numbers)
        case s @ Engine.Symbol(point, _, _) =>
          copy(symbols = symbols + (point -> s))

    def calculateP1(): Long =
      numbers
        .filter(_.adjacent.exists(symbols.contains))
        .map(_.value)
        .sum

    def calculateP2(): Long =
      symbols
        .filter(_._2.value == '*')
        .keys
        .toList
        .map(star => numbers.filter(_.adjacent.contains(star)))
        .collect { case List(x, y) => x.value * y.value }
        .sum

  object Schematic:
    val empty = Schematic(List.empty, Map.empty)

  def parse(str: String, y: Int): List[Engine] =
    lazy val ignore = P.char('.')
    lazy val number = (ignore.rep0.with1 *> (P.caret.with1 ~ digits) <* ignore.rep0).map: (caret, value) =>
      Engine.Number(Point(caret.col, y), value.length, value.toLong)
    lazy val symbolChars = P.charWhere(!"0123456789.".contains(_))
    lazy val symbol = (ignore.rep0.with1 *> (P.caret.with1 ~ symbolChars) <* ignore.rep0).map: (caret, value) =>
      Engine.Symbol(Point(caret.col, y), 1, value)
    lazy val engines = (number.backtrack | symbol).rep0
    engines.parseAll(str).toOption.get
