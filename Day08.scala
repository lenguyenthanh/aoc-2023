package aoc

import cats.effect.*
import fs2.Stream

object Day08 extends AOCApp(2023, 8):

  def part1(input: Stream[IO, String]): IO[String] = solve(input, _.solveP1)
  def part2(input: Stream[IO, String]): IO[String] = solve(input, _.solveP2)

  def solve(input: Stream[IO, String], f: Solution => Any): IO[String] =
    input.map(Parser.parse).map(f).map(_.toString).compile.lastOrError

  case class Pair(left: String, right: String)
  enum Direction:
    case Left, Right

  case class Solution(instruction: List[Direction], map: Map[String, Pair]):

    @annotation.tailrec
    private def loop(left: List[Direction], count: Int, current: String, end: String => Boolean): Int =
      if end(current) then count
      else
        val pair = map(current)
        val rest = if left.isEmpty then instruction else left
        val next = if rest.head == Direction.Left then pair.left else pair.right
        loop(rest.tail, count + 1, next, end)

    def solveP1: Int = loop(instruction, 0, "AAA", _ == "ZZZ")

    def solveP2: BigInt =
      map.keys
        .filter(_.endsWith("A"))
        .toList
        .map(loop(instruction, 0, _, _.endsWith("Z")))
        .map(BigInt(_))
        .foldLeft(BigInt(1))((acc, x) => lcm(acc, x))

    @annotation.tailrec
    private def gdc(a: BigInt, b: BigInt): BigInt =
      if b == 0 then a else gdc(b, a % b)

    private def lcm(a: BigInt, b: BigInt): BigInt =
      (a * b) / gdc(a, b)

  object Parser:
    def parse(input: String): Solution =
      input.split("\n\n").toList match
        case x :: y :: Nil =>
          Solution(parseInstruction(x), y.split("\n").toList.map(parsePair).toMap)

    val parseInstruction: String => List[Direction] = _.toList.map:
      case 'L' => Direction.Left
      case 'R' => Direction.Right

    val parsePair: String => (String, Pair) = _ match
      case s"$key = ($left, $right)" => key -> Pair(left, right)
