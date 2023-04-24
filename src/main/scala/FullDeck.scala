package skat.datatype

import scala.annotation.tailrec
import scala.util.Random
import zio.prelude.*
import zio.prelude.fx.ZPure
import skat.Skat.GameState.*

opaque type FullDeck = Seq[Card]

object FullDeck:
  import Card.Face
  import Card.Suite
  // import Card.Color

  def apply: FullDeck =
    for {
      k <- Face.values.toSeq
      s <- Card.Suite.values.toSeq
      // c <- Color.values.toSeq
    } yield Card(k, s)

  @tailrec
  // todo remove random side effect
  def suffle(round: Int)(fd: FullDeck): FullDeck =
    if (round <= 0) fd
    else suffle(round - 1)(fd.zip(List.fill(fd.size)(Random.nextInt(fd.size))).sortBy(_._2).map(_._1))

  def split(fullDeck: FullDeck): (SkatDeck, PlayerDecks) =
    def splitInto(cards: Seq[Card], groups: List[Int]): List[Seq[Card]] =
      groups.foldLeft((List.empty[Seq[Card]], cards)) { case ((splits, from), size) =>
        val (split, tail) = from.splitAt(size)
        (splits :+ split, tail)
      }._1

    def playerDecks(one: Seq[Card], two: Seq[Card], three: Seq[Card]): PlayerDecks =
      // TODO not working, why
      // (one.sliding(3, 3).toList, two.sliding(4, 3).toList, three.sliding(3, 3).toList).mapN(_ ++ _ ++ _) match
      (one.sliding(3, 3) zip three.sliding(3, 3) zip two.sliding(4,3)).map{ case ((a,b),c) => a ++ b ++ c}.toList match
        case pd1 :: pd2 :: pd3 :: Nil => PlayerDecks(pd3, pd1, pd2)
        case _                        => PlayerDecks(Seq.empty, Seq.empty, Seq.empty) // just to make compiler happy

    splitInto(fullDeck, List(9, 2, 12, 9)) match
      case one :: skat :: two :: three :: Nil => (skat, playerDecks(one, two, three))
      case _                                  => (Seq.empty, PlayerDecks(Seq.empty, Seq.empty, Seq.empty))

  object syntax:
    extension (deck: FullDeck)
      def suffle(round: Int): FullDeck   = FullDeck.suffle(round)(deck)
      def split: (SkatDeck, PlayerDecks) = FullDeck.split(deck)
