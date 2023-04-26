package skat.datatype
import zio.prelude.Reader
import zio.prelude.fx.ZPure
import zio.prelude._

trait TripelN[A, B, C, D]:
  def tripel(a: A): (B, C, D)
  def apply(t:(B,C,D)): A

type Tripel[A,B] = TripelN[A,B,B,B]

type PlayerDeck = Seq[Card] //10
object PlayerDeck:
  def add(pd: PlayerDeck, cards: Seq[Card]): Seq[Card]    = pd ++ cards
  def remove(pd: PlayerDeck, cards: Seq[Card]): Seq[Card] = pd.filterNot(cards.contains(_))

  object Syntax:
    extension (pd: PlayerDeck)
      def add(cards: Seq[Card])    = PlayerDeck.add(pd, cards)
      def remove(cards: Seq[Card]) = PlayerDeck.remove(pd, cards)

case class PlayerDecks(dealer: PlayerDeck, listener: PlayerDeck, speaker: PlayerDeck)
object PlayerDecks:
  import Players.Syntax.*

  given Tripel[PlayerDecks, PlayerDeck] with
    def tripel(pds: PlayerDecks): (PlayerDeck, PlayerDeck, PlayerDeck) = (pds._1, pds._2, pds._3)
    def apply(t:(PlayerDeck,PlayerDeck,PlayerDeck)): PlayerDecks = PlayerDecks.apply.tupled(t)

  object Syntax:
    extension (pds: PlayerDecks)
      def ppds: ZPure[Nothing, Any, Any, Players, PlayerError, List[(Player, PlayerDeck)]] =
        Players.getAll(pds)

      def updated(player: Player)(update: PlayerDeck => PlayerDeck): ZPure[Nothing, Nothing, Nothing, Players, PlayerError, PlayerDecks] =
        Players.mapAll[PlayerDecks,PlayerDeck]((p,pd) => if player == p then update(pd) else pd)(pds)
