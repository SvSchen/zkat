package skat.datatype
import zio.prelude.fx.ZPure

sealed trait PlayerError
case object NoPlayerBidding extends PlayerError

case class Player(name: String) {
  def requestBiddingValue(value: BiddingValue): ZPure[Nothing, Any, Any, Any, Option[PlayerError], BiddingValue] = ???
  def accept(value: BiddingValue): ZPure[Nothing, Any, Any, Any, Option[PlayerError], BiddingValue]              = ???
  def requestTrump(skat: SkatDeck): ZPure[Nothing, Any, Nothing, Any, PlayerError, (Trump, SkatDeck)]            = ???
  def requestTrick(pcd: PlayerCardDeck,t: Trump): ZPure[Nothing, Any, Any, Any, PlayerError, Card]                    = ???
  def set(pd: PlayerDeck): ZPure[Nothing, Any, Any, Any, PlayerError, Unit]                                      =
    ZPure.succeed(println(s"player: $name -> $pd"))
}

final case class Players(dealer: Player, listener: Player, speaker: Player):
  val size                        = this.productArity
  def leftNeighbor(p: Player): Player =
    if p == dealer then speaker
    else if p == listener then dealer
    else listener

object Players:
  def zip[A, B, C, D](p: Players, a: A)(using at: Tripel[A, B, C, D]): ((Player, B), (Player, C), (Player, D)) =
    (p._1, p._2, p._3) zip at.tripel(a)

  object Syntax:
    extension (ps: Players)
      def zip[A, B, C, D](a: A)(using at: Tripel[A, B, C, D]): ((Player, B), (Player, C), (Player, D)) =
        Players.zip(ps, a)
