package skat

import zio.prelude.fx.ZPure

import zio.prelude.*
import skat.datatype.*

object Skat:
  enum GameState:
    case DealS(sd: SkatDeck, pds: PlayerDecks)                         extends GameState
    case TrumpS(sd: SkatDeck, usd: SkatDeck, pds: PlayerDecks)         extends GameState
    case PlayS(sd: SkatDeck, pds: PlayerDecks, ptds: PlayerTrickDecks) extends GameState

  import GameState.*

  def deal: ZPure[Nothing, FullDeck, DealS, Players, PlayerError, Unit] =
    import FullDeck.syntax.*
    import PlayerDecks.Syntax.*
    for {
      ds   <- ZPure.get[DealS].contramapState[FullDeck](_.suffle(4).split |> DealS.apply)
      ppds <- ds.pds.ppds
      _    <- ZPure.foreach(ppds)((p, pd) => p.set(pd)).asState(ds)
    } yield ()

  def bid: ZPure[Nothing, DealS, DealS, Players, PlayerError, (Player, BiddingValue)] =
    for {
      ps    <- ZPure.service[DealS, Players]
      dealS <- ZPure.get[DealS]
      t     <- Bidding.bidding(ps.speaker, ps.listener, BiddingValue.values).asState(dealS)
    } yield t

  def selectTrump(declarer: Player): ZPure[Nothing, DealS, TrumpS, Players, PlayerError, Trump] =
    import PlayerDecks.Syntax.*
    import PlayerDeck.Syntax.*
    for {
      ps                  <- ZPure.service[DealS, Players]
      ds                  <- ZPure.get[DealS]
      t                   <- declarer.requestTrump(ds.sd)
      (trump, updatedSkat) = t
      updatedPds          <- ds.pds.updated(declarer)(_.add(ds.sd).remove(updatedSkat))
      _                   <- ZPure.update[DealS, TrumpS](ds => TrumpS(ds.sd, updatedSkat, updatedPds))
    } yield trump

  def runTricks(t: Trump): ZPure[Nothing, TrumpS, PlayS, Players, PlayerError, Unit] =
    for {
      ps          <- ZPure.service[TrumpS, Players]
      ts          <- ZPure.get[TrumpS]
      wtds        <- ZPure
                       .foreach(0 until ts.pds.dealer.size)(_ => Tricks.trick(t))
                       .provideService(ps)
                       .runAll(ps.speaker)
                       ._2
                       .fold(ZPure.failCause(_), ZPure.succeed(_))
      wtdsByPlayer = wtds._2.groupBy(_._1).mapValues(_.map(_._2))
      ptds         = PlayerTrickDecks(
                       Tricks.tricksOf(ps.dealer, wtdsByPlayer),
                       Tricks.tricksOf(ps.listener, wtdsByPlayer),
                       Tricks.tricksOf(ps.speaker, wtdsByPlayer)
                     )
      _           <- ZPure.update[TrumpS,PlayS](ts => PlayS(ts.sd, ts.pds, ptds))
    } yield ()

  def game: ZPure[Nothing, FullDeck, PlayS, Players, PlayerError, Unit] =
    // : ZPure[Nothing, FullDeck, PlayS, Players, Option[String] | String, (Player, BiddingValue, Trump, DeclarerPoints)] =
    for {
      _                       <- deal
      t                       <- bid
      (declarer, biddingValue) = t
      trump                   <- selectTrump(declarer)
      s                       <- runTricks(trump).getState.map(_._1)
      _ = println(s.sd)
      _ = println()
      _ = println(s.pds)
      _ = println()
      _ = println(s.ptds)
      // points                  <- Result.calc(declarer, biddingValue, trump)
      // } yield (declarer, biddingValue, trump, points)
    } yield ()

@main def xx =
  val d = new Player("dealer"):
    var pd = Seq.empty[Card]
    override def set(pd:PlayerDeck)= 
      this.pd = pd 
      ZPure.unit
    override def requestBiddingValue(
      value: BiddingValue
    ): ZPure[Nothing, Any, Any, Any, Option[PlayerError], BiddingValue] = ZPure.fail(None)
    override def accept(value: BiddingValue): ZPure[Nothing, Any, Any, Any, Option[PlayerError], BiddingValue] =
      ZPure.fail(None)
    override def requestTrick(pcd: PlayerCardDeck,t:Trump): ZPure[Nothing,Any,Any, Any, PlayerError, Card] =
      val c=pd.head
      pd = pd.tail
      ZPure.succeed(c)

  val l = new Player("listener"):
    var pd = Seq.empty[Card]
    override def set(pd:PlayerDeck)= 
      this.pd = pd 
      ZPure.unit
    override def requestBiddingValue(
      value: BiddingValue
    ): ZPure[Nothing, Any, Any, Any, Option[PlayerError], BiddingValue] = ZPure.succeed[Any, BiddingValue](value)
    override def accept(value: BiddingValue): ZPure[Nothing, Any, Any, Any, Option[PlayerError], BiddingValue] =
      ZPure.fail(None)
    override def requestTrump(
      skat: SkatDeck
    ): ZPure[Nothing, Any, Nothing, Any, PlayerError, (Trump, SkatDeck)] = ZPure
      .succeed((Null(Announcement.No, false), skat))
      .asInstanceOf[ZPure[Nothing, Any, Nothing, Any, PlayerError, (Trump, SkatDeck)]]
    override def requestTrick(pcd: PlayerCardDeck,t:Trump): ZPure[Nothing,Any,Any, Any, PlayerError, Card] =
      val c=pd.head
      pd = pd.tail
      ZPure.succeed(c)

  val s = new Player("speaker"):
    var pd = Seq.empty[Card]
    override def set(pd:PlayerDeck)= 
      this.pd = pd 
      ZPure.unit
    override def requestBiddingValue(
      value: BiddingValue
    ): ZPure[Nothing, Any, Any, Any, Option[PlayerError], BiddingValue] = ZPure.fail(None)
    override def accept(value: BiddingValue): ZPure[Nothing, Any, Any, Any, Option[PlayerError], BiddingValue] =
      ZPure.fail(None)
    override def requestTrick(pcd: PlayerCardDeck,t:Trump): ZPure[Nothing,Any,Any, Any, PlayerError, Card] =
      val c=pd.head
      pd = pd.tail
      ZPure.succeed(c)

  Skat.game.provideService(Players(d, l, s)).runAll(FullDeck.apply)
