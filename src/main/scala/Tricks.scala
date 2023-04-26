package skat.datatype

import zio.prelude._
import zio.prelude.fx.ZPure

import scala.collection.MapView

type TrickDeck       = Seq[Card]
type PlayerTrickDeck = Seq[TrickDeck]
type PlayerCard      = (Player, Card)
type WinnerTrickDeck = (Player, Seq[Card])
type PlayerCardDeck  = Seq[PlayerCard]
object PlayerCardDeck:
  extension (pcd: PlayerCardDeck) def winnerBy(t: Trump): Player = 
    val pcdt = t match
      // no trump is provided in this deck, then the first card defined the trump for this deck
      case Suite(s,a,b) if pcd.find(_._2.suite == s).isEmpty => Suite(pcd.head._2.suite,a,b)
      case _ => t
    pcdt.sortBy[PlayerCard](_._2)(pcd).last._1

case class PlayerTrickDecks(dealer: PlayerTrickDeck, listener: PlayerTrickDeck, speaker: PlayerTrickDeck)
object PlayerTrickDecks:
  given Tripel[PlayerTrickDecks, PlayerTrickDeck] with
    def tripel(ptds: PlayerTrickDecks): (PlayerTrickDeck, PlayerTrickDeck, PlayerTrickDeck) = (ptds._1, ptds._2, ptds._3)
    def apply(t:(PlayerTrickDeck,PlayerTrickDeck,PlayerTrickDeck)): PlayerTrickDecks = PlayerTrickDecks.apply.tupled(t)

object Tricks:
  import PlayerCardDeck.*
  type PlayerCardState = (Player, PlayerCardDeck)

  def playerCard(t: Trump): ZPure[Nothing, PlayerCardState, PlayerCardState, Players, PlayerError, Unit] =
    for {
      ps      <- ZPure.service[PlayerCardState, Players]
      pcs     <- ZPure.get[PlayerCardState]
      (p, pcd) = pcs
      c       <- p.requestTrick(pcd,t)
      _       <- ZPure.update(pcs => (ps.leftNeighbor(p), (p, c) +: pcd))
    } yield ()

  def trick(t: Trump): ZPure[Nothing, Player, Player, Players, PlayerError, WinnerTrickDeck] =
    for {
      ps  <- ZPure.service[Player, Players]
      p   <- ZPure.get[Player]
      pcd <- ZPure
               .foreach(0 until ps.size)(_ => playerCard(t))
               .provideService(ps)
               .runAll((p, Seq.empty[PlayerCard]))
               ._2
               .fold(ZPure.failCause(_), t => ZPure.succeed(t._1._2.reverse))
      w    = pcd.winnerBy(t)
      _ = println(s"pcd: $pcd winner: $w")
      _   <- ZPure.set(w)
    } yield (w, pcd.map(_._2))

  def tricksOf(p: Player, from: MapView[Player, PlayerTrickDeck]): PlayerTrickDeck =
    from.getOrElse(p, Seq.empty[TrickDeck])
