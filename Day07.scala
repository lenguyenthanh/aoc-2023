package aoc

import cats.effect.*
import fs2.Stream

object Day07 extends AOCApp(2023, 7):

  def part1(input: Stream[IO, String]): IO[String] =
    input
      .mapInput(Parser.parseLine)
      .map(Solution(_))
      .map(_.solveP1())
      .map(_.toString)

  def part2(input: Stream[IO, String]): IO[String] =
    input
      .mapInput(Parser.parseLine)
      .map(Solution(_))
      .map(_.solveP2())
      .map(_.toString)

  enum Card:
    case A, K, Q, J, T, Nine, Eight, Seven, Six, Five, Four, Three, Two

  enum Kind:
    case Five, Four, FullHouse, Three, TwoPair, Pair, HighCard

  object Card:
    def compareP1(x: Card, y: Card): Int = -x.ordinal.compareTo(y.ordinal)
    def compareP2(x: Card, y: Card): Int =
      if x == Card.J && y == Card.J then 0
      else if x == Card.J then -1
      else if y == Card.J then 1
      else -x.ordinal.compareTo(y.ordinal)

  object Kind:
    def compare(x: Kind, y: Kind): Int = -x.ordinal.compareTo(y.ordinal)

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
      val firstKind = kindP1
      if !cards.exists(_ == Card.J) then firstKind
      else if firstKind == Kind.Five then Kind.Five
      else if firstKind == Kind.Four then Kind.Five
      else if firstKind == Kind.FullHouse then Kind.Five
      else if firstKind == Kind.Three then Kind.Four
      else if firstKind == Kind.Pair then Kind.Three
      else if firstKind == Kind.HighCard then Kind.Pair
      else
        val countJ = cards.count(_ == Card.J)
        if countJ == 2 then Kind.Four
        else Kind.FullHouse

  case class Solution(hands: List[Hand]):
    def solveP1(): Long =
      given Ordering[Hand] with
        def compare(left: Hand, right: Hand): Int =
          val kindOrder = Kind.compare(left.kindP1, right.kindP1)
          if kindOrder != 0 then kindOrder
          else
            val cardsOrder = left.cards.zip(right.cards).map((a, b) => Card.compareP1(a, b)).find(_ != 0)
            cardsOrder.getOrElse(0)

      hands.zipWithIndex.foldLeft(0L):
        case (acc, (hand, index)) => acc + hand.bid * (index + 1)

    def solveP2(): Long =
      given Ordering[Hand] with
        def compare(left: Hand, right: Hand): Int =
          val kindOrder = Kind.compare(left.kindP2, right.kindP2)
          if kindOrder != 0 then kindOrder
          else
            val cardsOrder = left.cards.zip(right.cards).map((a, b) => Card.compareP2(a, b)).find(_ != 0)
            cardsOrder.getOrElse(0)

      hands.sorted.zipWithIndex.foldLeft(0L):
        case (acc, (hand, index)) => acc + hand.bid * (index + 1)

  object Parser:
    def parse(input: String): Solution = Solution(Nil)

    def parseLine(line: String): Hand = line match
      case s"$cs $b" =>
        val cards = cs.map(parseCard).toList
        val bid   = b.toLong
        Hand(cards, bid)

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
