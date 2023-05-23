package skat.datatype

import skat.Skat.GameState.*
import zio.prelude.AssociativeBothOps
import zio.prelude._
import zio.prelude.fx.ZPure

case class DeclarerPoints(value: Int) extends AnyVal

object Result:
  import Announcement.*
  import Players.Syntax.*
  import PlayerTrickDecks.*
  val faceDeckWithoutJack                               = Card.Face.values.filterNot(_ == Card.Face.Jack)
  def jackCount(ptd: PlayerTrickDeck): Int              =
    val jackDeck = ptd.flatten.filter(_.face == Card.Face.Jack)
    jackDeck.sortBy(_.suite.ordinal).lastOption.fold(4)(_.suite.ordinal + 1)
  def matadorCount(ptd: PlayerTrickDeck, s: Suite): Int =
    val suiteDeck = ptd.flatten.filterNot(_.face == Card.Face.Jack).filter(_.suite == s.suite)
    (s.sort(suiteDeck.reverse) zip faceDeckWithoutJack.reverse).filter((c, f) => c.face == f).size

  def winSimple(sum: Int): Option[Int]           = Option.when(sum > 60)(No.value)
  def winSchneider(sum: Int): Option[Int]        = Option.when(sum >= 90)(Schneider.value)
  def winSchwarz(size: Int): Option[Int]         = Option.when(size == 10)(Schwarz.value)
  def winOvert(size: Int): Option[Int]           = winSchwarz(size).as(Ouvert.value)
  def withHand(b: Boolean, a: Announcement): Int =
    if b then a.value else 0

  def calc(
    declarer: Player,
    bv: BiddingValue,
    t: Trump
  ): ZPure[Nothing, PlayS, PlayS, Players, PlayerError, DeclarerPoints] =
    // val opponentDeckSum: Sum[Int]     = Sum(120 - declarerDeckSum)

    def winValue(mastadorCount: Int, hand: Int, wo: Option[Int]): Either[Int, Int] =
      wo.map(_ + hand + mastadorCount)
        .filter(_ >= bv)
        .fold(Left(hand + mastadorCount + 1))(Right(_))

    def const[A](aa: A)(a: A): A = aa
    def lose(i: Int): Int        = -2 * i

    (for {
      playS               <- ZPure.get[PlayS]
      declarerTrickDeck   <- Players.get(declarer, playS.ptds).asState(playS)
      declarerTrickDeckSum = declarerTrickDeck.flatten.map(_.face.value).sum
    } yield t match
      case Null(Ouvert, false)            => winSchwarz(declarerTrickDeck.size).fold(lose(46))(const(46))
      case Null(Ouvert, true)             => winSchwarz(declarerTrickDeck.size).fold(lose(59))(const(59))
      case Null(_, false)                 => winSchwarz(declarerTrickDeck.size).fold(lose(23))(const(23))
      case Null(_, true)                  => winSchwarz(declarerTrickDeck.size).fold(lose(35))(const(35))
      case Grand(No, hand)                =>
        winValue(
          jackCount(declarerTrickDeck),
          withHand(hand, No),
          winSchwarz(declarerTrickDeck.size) orElse winSchneider(declarerTrickDeckSum) orElse winSimple(
            declarerTrickDeckSum
          )
        ).fold(lose, identity) * Grand.value
      case Grand(Schneider, hand)         =>
        winValue(
          jackCount(declarerTrickDeck),
          withHand(hand, Schneider),
          winSchwarz(declarerTrickDeck.size) orElse winSchneider(declarerTrickDeckSum)
        ).fold(lose, identity) * Grand.value
      case Grand(Schwarz, hand)           =>
        winValue(
          jackCount(declarerTrickDeck),
          withHand(hand, Schwarz),
          winSchwarz(declarerTrickDeck.size)
        ).fold(lose, identity) * Grand.value
      case Grand(Ouvert, hand)            =>
        winValue(
          jackCount(declarerTrickDeck),
          withHand(hand, Ouvert),
          winOvert(declarerTrickDeck.size)
        ).fold(lose, identity) * Grand.value
      case st @ Suite(s, No, hand)        =>
        winValue(
          jackCount(declarerTrickDeck) + matadorCount(declarerTrickDeck, st),
          withHand(hand, No),
          winSchwarz(declarerTrickDeck.size) orElse winSchneider(declarerTrickDeckSum) orElse winSimple(
            declarerTrickDeckSum
          )
        ).fold(lose, identity) * s.value
      case st @ Suite(s, Schneider, hand) =>
        winValue(
          jackCount(declarerTrickDeck) + matadorCount(declarerTrickDeck, st),
          withHand(hand, Schneider),
          winSchwarz(declarerTrickDeck.size) orElse winSchneider(declarerTrickDeckSum)
        ).fold(lose, identity) * s.value
      case st @ Suite(s, Schwarz, hand)   =>
        winValue(
          jackCount(declarerTrickDeck) + matadorCount(declarerTrickDeck, st),
          withHand(hand, Schwarz),
          winSchwarz(declarerTrickDeck.size)
        ).fold(lose, identity) * s.value
      case st @ Suite(s, Ouvert, hand)    =>
        winValue(
          jackCount(declarerTrickDeck) + matadorCount(declarerTrickDeck, st),
          withHand(hand, Ouvert),
          winOvert(declarerTrickDeck.size)
        ).fold(lose, identity) * s.value
    ).map(DeclarerPoints(_))
