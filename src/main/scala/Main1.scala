package skat

import zio.prelude.fx.ZPure
import zio.prelude.*
import skat.datatype.*

object Skat:
  type State[SA,SB, +E, +A] = zio.prelude.fx.ZPure[Nothing, SA, SB, Any, E, A]

  enum GameState:
    case DealS(sd:SkatDeck,pds:PlayerDecks) extends GameState
    case BidS(sd:SkatDeck,pds:PlayerDecks,declarer:Player,bv:BiddingValue) extends GameState
    case TrumpS(sd:SkatDeck,usd:SkatDeck,pds:PlayerDecks,declarer:Player,bv:BiddingValue,t:Trump) extends GameState
    case PlayS(sd:SkatDeck,pds:PlayerDecks,ptds:PlayerTrickDecks,declarer:Player,bv:BiddingValue,t:Trump) extends GameState
    case ResultS(sd:SkatDeck,pds:PlayerDecks,ptds:PlayerTrickDecks,declarer:Player,bv:BiddingValue,t:Trump, dp: DeclarerPoints) extends GameState

  import GameState.*
  import FullDeck.syntax.*

  // todo refactor to simpler states
  def deal: State[FullDeck,DealS,Nothing,Unit] = ZPure.update(_.suffle(4).split |> DealS.apply)
  def bid(players:Players): State[DealS,BidS,String,Unit] = ZPure.update(ds => Bidding.run(players,ds.pds) |> ((declarer,bv) => BidS(ds.sd,ds.pds,declarer,bv)))
  def selectTrump: State[BidS,TrumpS,Nothing,Unit] = ZPure.update(bs => Trump.selectTrump(bs.declarer,bs.sd,bs.pds) |> ((updatedSkat,updatedPds,trump) => TrumpS(bs.sd,updatedSkat,updatedPds,bs.declarer,bs.bv,trump)))
  def play(players:Players):State[TrumpS,PlayS,Nothing,Unit] = ZPure.update(ts => Tricks.run(players,ts.pds,ts.usd,ts.t) |> ((ptds) => PlayS(ts.sd,ts.pds,ptds,ts.declarer,ts.bv,ts.t)))
  def result:State[PlayS,ResultS,Nothing,Unit] = ZPure.update(ps => Result.calc(ps.ptds,ps.declarer,ps.bv,ps.t) |> ((declarerPoints) => ResultS(ps.sd,ps.pds,ps.ptds,ps.declarer,ps.bv,ps.t,declarerPoints)))

  def game(players:Players) =
    for {
      _ <- deal
      x <- bid(players)
      _ <- selectTrump
      _ <- play(players)
      _ <- result
    } yield ()

  def xx =
    val x = game(null).runAll(FullDeck.apply)
  // case class Table(players: Players):
  //   def deal(deck: FullDeck): BidTable =
  //     Table.deal(players, deck)
