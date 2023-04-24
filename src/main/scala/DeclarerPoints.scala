package skat.datatype

import zio.prelude._
import zio.prelude.fx.ZPure

import zio.prelude.AssociativeBothOps
import zio.prelude.newtypes.Sum
import zio.prelude.newtypes.Prod

import skat.Skat.GameState.*


case class DeclarerPoints(value: Int) extends AnyVal

object Result:
  import Announcement.*
  def findMatadors: Seq[Card]                      = ???
  def matadorCount(ptd: PlayerTrickDeck): Sum[Int] = ???
  def winSimple(sum: Sum[Int]): Option[Int]        = Option.when(sum > 60)(No.value)
  def winSchneider(sum: Sum[Int]): Option[Int]     = Option.when(sum >= 90)(Schneider.value)
  def winSchwarz(size: Int): Option[Int]           = Option.when(size == 10)(Schwarz.value)
  def winOvert(size: Int): Option[Int]             = winSchwarz(size).as(Ouvert.value)
  def withHand(b: Boolean): Int                    = if b then Sum(1) else Sum(0)

  def calc(declarer: Player, bv: BiddingValue, t: Trump): ZPure[Nothing,PlayS,PlayS,Players,Nothing,DeclarerPoints] =
    val declarerDeckSum: Sum[Int]     = ???
    val opponentDeckSum: Sum[Int]     = Sum(120 - declarerDeckSum)
    val declarerDeck: PlayerTrickDeck = ???

    def winValue(declarerDeck: PlayerTrickDeck, hand: Boolean, wo: Option[Int]): Either[Int, Int] =
      val h = withHand(hand)
      val m = matadorCount(declarerDeck)
      wo.map(_ + h + m)
        .filter(_ >= bv)
        .fold(Left(h + m))(Right(_))

    def const[A](aa: A)(a: A): A               = aa
    def lose(i: Int): Int                      = -2 * i
    def loseWith(a: Announcement)(i: Int): Int = lose(a.value + i)
    def winWith(a: Announcement)(i: Int): Int  = a.value + i

    ZPure.serviceWith[Players](ps =>
      t match
        case Null(Ouvert, false)       => winSchwarz(declarerDeck.size).fold(lose(46))(const(46))
        case Null(Ouvert, true)        => winSchwarz(declarerDeck.size).fold(lose(59))(const(59))
        case Null(_, false)            => winSchwarz(declarerDeck.size).fold(lose(23))(const(23))
        case Null(_, true)             => winSchwarz(declarerDeck.size).fold(lose(35))(const(35))
        case Suite(s, No, hand)        =>
          winValue(
            declarerDeck,
            hand,
            winSchwarz(declarerDeck.size) orElse winSchneider(declarerDeckSum) orElse winSimple(declarerDeckSum)
          ).fold(lose, identity) * s.value
        case Grand(No, hand)           =>
          winValue(
            declarerDeck,
            hand,
            winSchwarz(declarerDeck.size) orElse winSchneider(declarerDeckSum) orElse winSimple(declarerDeckSum)
          ).fold(lose, identity) * Grand.value
        case Suite(s, Schneider, hand) =>
          winValue(declarerDeck, hand, winSchwarz(declarerDeck.size) orElse winSchneider(declarerDeckSum))
            .fold(loseWith(Schneider), winWith(Schneider)) * s.value
        case Grand(Schneider, hand)    =>
          winValue(declarerDeck, hand, winSchwarz(declarerDeck.size) orElse winSchneider(declarerDeckSum))
            .fold(loseWith(Schneider), winWith(Schneider)) * Grand.value
        case Suite(s, Schwarz, hand)   =>
          winValue(declarerDeck, hand, winSchwarz(declarerDeck.size))
            .fold(loseWith(Schwarz), winWith(Schwarz)) * s.value
        case Grand(Schwarz, hand)      =>
          winValue(declarerDeck, hand, winSchwarz(declarerDeck.size))
            .fold(loseWith(Schwarz), winWith(Schwarz)) * Grand.value
        case Suite(s, Ouvert, hand)    =>
          winValue(declarerDeck, hand, winOvert(declarerDeck.size)).fold(loseWith(Ouvert), winWith(Ouvert)) * s.value
        case Grand(Ouvert, hand)       =>
          winValue(declarerDeck, hand, winOvert(declarerDeck.size))
            .fold(loseWith(Ouvert), winWith(Ouvert)) * Grand.value
    ).map(DeclarerPoints(_))
