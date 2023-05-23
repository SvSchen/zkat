package skat.datatype

import zio.prelude.fx.ZPure
import zio.prelude.*

object BiddingValue extends Subtype[Int]:
  // override inline def assertion = Assertion.between(1000, 9999)
  val values: NonEmptyList[BiddingValue] =
    val suiteBv =
      for {
        ss <- Card.Suite.values.map(_.value).toList
        ms <- 2 to 18
      } yield (ss * ms)
    val grandBv = 264 +: (1 to 10).map(_ * Grand.value)
    (suiteBv ++ grandBv ++ Null.values).sorted.distinct.map(BiddingValue(_)).toNonEmptyList.get

  val min: BiddingValue = values.head

type BiddingValue = BiddingValue.Type

object Bidding:
  private[datatype] def ask(
    speaker: Player,
    bv: BiddingValue
  ): ZPure[Nothing, Any, Any, Players, PlayerError, (Player, BiddingValue)] =
    def request(
      p: Player,
      bv: BiddingValue
    ): ZPure[Nothing, Any, Any, Any, Option[PlayerError], (Player, BiddingValue)] =
      p.requestBiddingValue(bv).map((p, _))

    ZPure.serviceWithPure[Players](ps =>
      val asked: ZPure[Nothing, Any, Any, Any, Option[PlayerError], (Player, BiddingValue)] = speaker match
        case ps.speaker  =>
          request(ps.speaker, bv) orElseOptional request(ps.dealer, bv) orElseOptional request(ps.listener, bv)
        case ps.dealer   => request(ps.dealer, bv) orElseOptional request(ps.listener, bv)
        case ps.listener => request(ps.listener, bv)

      asked.catchAll {
        case None    => ZPure.fail(skat.datatype.NoPlayerBidding)
        case Some(x) => ZPure.fail(x)
      }
    )

  private[datatype] def accept(
    listener: Player
  )(
    speaker: Player,
    bv: BiddingValue
  ): ZPure[Nothing, Any, Any, Players, PlayerError, (Player, Player, BiddingValue)] =
    def request(p: Player, bv: BiddingValue): ZPure[Nothing, Any, Any, Any, Option[PlayerError], (Player, BiddingValue)] =
      p.accept(bv).map((p, _))

    ZPure.serviceWithPure[Players](ps =>
      val accepted = (speaker, listener) match
        case (ps.speaker, ps.listener) => request(listener, bv) orElseOptional request(ps.dealer, bv)
        case (ps.speaker, ps.dealer)   => request(listener, bv)
        case (ps.dealer, ps.listener)  => request(listener, bv)
        case _                         => ZPure.fail(None)
      accepted.map(t => (speaker, t._1, t._2)) orElse ZPure.succeed((speaker, speaker, bv))
    )

  def bidding(
    speaker: Player,
    listener: Player,
    bvs: List[BiddingValue]
  ): ZPure[Nothing, Any, Any, Players, PlayerError, (Player, BiddingValue)] =
    ask(speaker, bvs.head)
      >>= (accept(listener).tupled)
      >>= (_ match
        case (s, l, v) if (s == l)        => ZPure.succeed((s, v))
        case (s, l, v) if (bvs.size == 1) => ZPure.succeed((l, v))
        case (s, l, v)                    => bidding(s, l, bvs.drop(1))
      )
