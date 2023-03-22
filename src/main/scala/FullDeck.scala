package skat.datatype

import scala.annotation.tailrec

opaque type FullDeck = Seq[Card]

object FullDeck:
  import Card.Kind
  import Card.Suite
  import Card.Color
  import scala.util.Random

  def apply: FullDeck =
    for {
      k <- Kind.values.toSeq
      s <- Card.Suite.values.toSeq
      c <- Color.values.toSeq
    } yield Card(k, s, c)

  @tailrec
  def suffle(round: Int)(deck: FullDeck): FullDeck =
    if (round <= 0) deck
    else suffle(round - 1)(deck.zip(List.fill(deck.size)(Random.nextInt(deck.size))).sortBy(_._2).map(_._1))

  def split(deck: FullDeck): (SkatDeck, PlayerDecks) =
    // @tailrec
    def take(cards: Seq[Card], size: List[Int]): List[Seq[Card]] = {
      size match
        case Nil       => Nil
        case h :: tail =>
          val (ret, splitTail) = cards.splitAt(h)
          List(ret) ++ take(splitTail, tail)
    }

    import zio.prelude.AssociativeBothTuple3Ops

    def playerDecks(one: Seq[Card], two: Seq[Card], three: Seq[Card]): PlayerDecks = {
    // import zio.prelude.AssociativeBoth.*
    //  val x = mapN(one.sliding(3, 3).toList,two.sliding(4, 3).toList,three.sliding(3, 3).toList)( _ ++ _ ++ _ )
      val y = (one.sliding(3, 3).toList,two.sliding(4, 3).toList,three.sliding(3, 3).toList).mapN( _ ++ _ ++ _ )
      y match {


      // (one.sliding(3, 3) zip two.sliding(4, 3) zip three.sliding(3, 3))
      //   .map(t => t._1._1 ++ t._1._2 ++ t._2)
        // .toList match 
        case pd1 :: pd2 :: pd3 :: Nil => PlayerDecks(pd1, pd2, pd3)
      }
    }

    take(deck, List(9, 2, 12, 9)) match {
      case one :: skat :: two :: three :: Nil => (skat, playerDecks(one, two, three))
    }

  object syntax:
    extension (deck: FullDeck)
      def suffle(round: Int): FullDeck   = FullDeck.suffle(round)(deck)
      def split: (SkatDeck, PlayerDecks) = FullDeck.split(deck)
