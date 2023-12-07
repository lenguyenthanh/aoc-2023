/*
borrowed from https://github.com/s5bug/aoc
 */

package aoc

import cats.*
import cats.effect.*
import cats.effect.std.*
import fs2.*
import org.http4s.*
import org.http4s.implicits.*
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.client.Client

abstract class AOCApp(year: Int, day: Int) extends IOApp:

  def part1(input: Stream[IO, String]): IO[String]
  def part2(input: Stream[IO, String]): IO[String]

  override def run(args: List[String]): IO[ExitCode] =
    val aocToken = IO(sys.env("AOC_SESSION_COOKIE"))

    EmberClientBuilder
      .default[IO]
      .build
      .use: client =>
        aocToken.flatMap: sessionCookie =>
          val req = Request[IO](
            uri = uri"https://adventofcode.com" / year.toString / "day" / day.toString / "input"
          ).addCookie("session", sessionCookie)
          val body = client.stream(req).flatMap(_.body).through(text.utf8.decode)
          part1(body).flatMap(Console[IO].println(_)) >>
            part2(body).flatMap(Console[IO].println(_))
      .as(ExitCode.Success)

  extension (input: Stream[IO, String])

    def run(f: fs2.Pipe[IO, String, Int]): IO[String] =
      input
        .through(fs2.text.lines)
        .filter(_.trim.nonEmpty)
        .through(f)
        .compile
        .lastOrError
        .map(_.toString)

    def mapInput[T](f: String => T): IO[List[T]] =
      input
        .through(fs2.text.lines)
        .filter(_.trim.nonEmpty)
        .map(f)
        .compile
        .toList
