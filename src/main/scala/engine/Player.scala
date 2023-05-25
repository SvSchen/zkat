package skat.datatype
import zio.prelude.fx.ZPure
import zio.prelude.*
import zio.Cause

sealed trait PlayerError
case object NoPlayerBidding          extends PlayerError
case class PlayerNotFound(p: Player) extends PlayerError
case class InputError(s: String)     extends PlayerError

trait Player(val name: String):
  def requestBiddingValue(value: BiddingValue): ZPure[Nothing, Any, Any, Any, Option[PlayerError], BiddingValue]
  def accept(value: BiddingValue): ZPure[Nothing, Any, Any, Any, Option[PlayerError], BiddingValue]
  def requestTrump(skat: SkatDeck): ZPure[Nothing, Any, Nothing, Any, PlayerError, (Trump, SkatDeck)]
  def requestTrick(pcd: PlayerCardDeck, t: Trump): ZPure[Nothing, Any, Any, Any, PlayerError, Card]
  def set(pd: PlayerDeck): ZPure[Nothing, Any, Any, Any, Nothing, Unit]

final case class Players(dealer: Player, listener: Player, speaker: Player):
  val size                            = this.productArity
  def leftNeighbor(p: Player): Player =
    if p == dealer then speaker
    else if p == listener then dealer
    else listener

object Players:
  def getAll[A, B](that: A)(using Tripel[A, B]): ZPure[Nothing, Any, Any, Players, Nothing, List[(Player, B)]] =
    ZPure.serviceWith(ps => zip(ps, that).productIterator.toList.asInstanceOf[List[(Player, B)]])

  def get[A, B](p: Player, that: A)(using Tripel[A, B]): ZPure[Nothing, Any, Any, Players, PlayerError, B] =
    getAll(that).flatMap(ts => ZPure.fromOption(ts.find(_._1 == p)).bimap(_ => PlayerNotFound(p), _._2))

  def mapAll[A, B](f: ((Player, B)) => B)(that: A)(using
    T: Tripel[A, B]
  ): ZPure[Nothing, Nothing, Nothing, Players, PlayerError, A] =
    ZPure.serviceWith[Players](ps => zip(ps, that) |> { (a, b, c) => T.apply((f(a), f(b), f(c))) })

  def zip[A, B, C, D](ps: Players, a: A)(using at: TripelN[A, B, C, D]): ((Player, B), (Player, C), (Player, D)) =
    (ps._1, ps._2, ps._3) zip at.tripel(a)

  object Syntax:
    extension (ps: Players)
      def zip[A, B, C, D](a: A)(using at: TripelN[A, B, C, D]): ((Player, B), (Player, C), (Player, D)) =
        Players.zip(ps, a)
      def toList: List[Player]                                                                          = ps.productIterator.toList.asInstanceOf[List[(Player)]]
