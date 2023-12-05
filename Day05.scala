package aoc

import cats.effect.*
import fs2.Stream

object Day05 extends AOCApp(2023, 5):

  def part1(input: Stream[IO, String]): IO[String] =
    runx(input, _.solveP1())

  def part2(input: Stream[IO, String]): IO[String] =
    input
      .map(Parser.parse)
      .flatMap(_.solveP2())
      .map(_.toString)
      .compile
      .lastOrError

  def runx(input: Stream[IO, String], f: Solution => Long): IO[String] =
    input
      .map(Parser.parse)
      .map(f)
      .map(_.toString)
      .compile
      .lastOrError

  case class Range(start: Long, end: Long, range: Long):
    def des(n: Long): Option[Long] =
      if start <= n && n < start + range then Some(end + (n - start))
      else None

  case class M(source: String, destination: String, data: List[Range]):
    def add(range: Range): M = copy(data = range :: data)
    def get(s: Long): Long =
      val x = data.foldLeft[Option[Long]](None):
                case (Some(x), _) => Some(x)
                case (None, r)    => r.des(s)
      x.getOrElse(s)

  object M:
    def apply(source: String, destination: String) = new M(source, destination, List.empty)

  case class Solution(seeds: List[Long], maps: List[M]):
    def solve(xs: List[Long]): Long =
      xs
        .map: s =>
          maps.foldLeft(s):
            case (acc, m) => m.get(acc)
        .min

    def solveP1(): Long =
      solve(seeds)

    def solveFaster(xs: Stream[IO, Long]): IO[Long] =
      xs
        .fold(Long.MaxValue): (min, s) =>
          val x = maps.foldLeft(s):
            case (acc, m) => m.get(acc)
          if x < min then x else min
        .compile
        .lastOrError

    def solveP2(): Stream[IO, Long] =
      val sx = moreSeeds()
      // println(sx.length)
      Stream.emits(sx)
      .parEvalMap(8)(x => solveFaster(x.toStream))
      .fold(Long.MaxValue)((min, x) => if x < min then x else min)

    def moreSeeds(): List[SeedRange] =
      @annotation.tailrec
      def loop(rest: List[Long], acc: List[SeedRange]): List[SeedRange] =
        rest match
          case Nil => acc
          case x :: y :: xs => loop(xs, SeedRange(x, y) :: acc)
      loop(seeds, Nil)

  case class SeedRange(start: Long, size: Long):
    def toStream = Stream.range(start, start + size)

  object Parser:
    def parse(input: String): Solution =
      input.split("\n\n").toList match
        case x :: y =>
          val seeds = parseSeeds(x)
          val maps  = y.map(parseMap)
          Solution(seeds, maps)

    val parseSeeds: String => List[Long] = _ match
      case s"seeds: $seeds" => seeds.split(" ").map(_.toLong).toList

    def parseMap(input: String): M =
      input.split("\n").toList match
        case x :: y =>
          val (s, d) = parseMapHeader(x)
          y.foldLeft(M(s, d)):
            case (acc, line) =>
              line.split(" ").map(_.toLong).toList match
                case start :: source :: range :: Nil =>
                  acc.add(Range(source, start, range))

    val parseMapHeader: String => (String, String) = _ match
      case s"$source-to-$destination map:" => (source, destination)

