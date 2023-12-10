package aoc

import cats.effect.*
import fs2.Stream

object Day07 extends AOCApp(2023, 7):

  def part1(input: Stream[IO, String]) = solve(_.solveP1())(input)
  def part2(input: Stream[IO, String]) = solve(_.solveP2())(input)

  def solve(f: Solution => Long): Stream[IO, String] => IO[String] =
    _.compileAsList(Parser.parseLine).map(Solution(_)).map(f).map(_.toString)

  enum Card:
    case A, K, Q, J, T, Nine, Eight, Seven, Six, Five, Four, Three, Two

    def compareP1(other: Card): Int = -ordinal.compareTo(other.ordinal)

    def compareP2(other: Card): Int =
      if this == Card.J && other == Card.J then 0
      else if this == Card.J then -1
      else if other == Card.J then 1
      else compareP1(other)

  enum Kind:
    case Five, Four, FullHouse, Three, TwoPair, Pair, HighCard
    def compare(other: Kind): Int = -ordinal.compareTo(other.ordinal)

  case class Hand(cards: List[Card], bid: Long):

    lazy val kindP1: Kind =
      val grouped = cards.groupMapReduce(identity)(_ => 1)(_ + _)
      if grouped.size == 1 then Kind.Five
      else if grouped.size == 2 then
        if grouped.values.exists(_ == 4) then Kind.Four
        else Kind.FullHouse
      else if grouped.size == 3 then
        if grouped.values.exists(_ == 3) then Kind.Three
        else Kind.TwoPair
      else if grouped.size == 4 then Kind.Pair
      else Kind.HighCard

    lazy val kindP2: Kind =
      if !cards.exists(_ == Card.J) then kindP1
      else if kindP1 == Kind.Five then Kind.Five
      else if kindP1 == Kind.Four then Kind.Five
      else if kindP1 == Kind.FullHouse then Kind.Five
      else if kindP1 == Kind.Three then Kind.Four
      else if kindP1 == Kind.Pair then Kind.Three
      else if kindP1 == Kind.HighCard then Kind.Pair
      else if cards.count(_ == Card.J) == 2
      then Kind.Four
      else Kind.FullHouse

  case class Solution(hands: List[Hand]):
    def solveP1(): Long =
      given Ordering[Hand] = ordering(_.kindP1, _.compareP1(_))
      count(hands.sorted)

    def solveP2(): Long =
      given Ordering[Hand] = ordering(_.kindP2, _.compareP2(_))
      count(hands.sorted)

    def ordering(k: Hand => Kind, c: (Card, Card) => Int): Ordering[Hand] =
      new Ordering[Hand]:
        def compare(left: Hand, right: Hand): Int =
          val kindOrder = k(left).compare(k(right))
          if kindOrder != 0 then kindOrder
          else
            left.cards
              .zip(right.cards)
              .map(c.tupled(_))
              .find(_ != 0)
              .getOrElse(0)

    val count: List[Hand] => Long =
      _.zipWithIndex.foldLeft(0L):
        case (acc, (hand, index)) => acc + hand.bid * (index + 1)

  object Parser:
    val parseLine: String => Hand = _ match
      case s"$cs $bid" => Hand(cs.map(parseCard).toList, bid.toLong)

    val parseCard: Char => Card = _ match
      case 'A' => Card.A
      case 'K' => Card.K
      case 'Q' => Card.Q
      case 'J' => Card.J
      case 'T' => Card.T
      case '9' => Card.Nine
      case '8' => Card.Eight
      case '7' => Card.Seven
      case '6' => Card.Six
      case '5' => Card.Five
      case '4' => Card.Four
      case '3' => Card.Three
      case '2' => Card.Two
