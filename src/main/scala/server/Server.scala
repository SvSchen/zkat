package server
import skat.datatype.Players
import zio.*
import zio.http.ChannelEvent.ChannelRead
import zio.http.ChannelEvent.ChannelRegistered
import zio.http.ChannelEvent.ChannelUnregistered
import zio.http.ChannelEvent.ExceptionCaught
import zio.http.ChannelEvent.UserEvent
import zio.http.ChannelEvent.UserEventTriggered
import zio.http.*
import zio.http.socket.*
import zio.stream.*
import skat.Skat
import skat.datatype.FullDeck
import skat.datatype.Trump
import skat.datatype.Player
import skat.datatype.BiddingValue
import skat.datatype.DeclarerPoints
import skat.datatype.PlayerError
import ws.WsPlayer
import zio.prelude.*

object SkatWebSocket extends ZIOAppDefault:

  def channelSocket[R](
    connections: Ref[Int],
    f: => Handler[Any, Throwable, (WebSocketChannel, String), Unit]
  ): Http[Any, Throwable, WebSocketChannelEvent, Unit] =
    Http.collectZIO[WebSocketChannelEvent] {
      case ChannelEvent(ch, ChannelRead(WebSocketFrame.Text(message)))        => f(ch, message)
      case ChannelEvent(ch, ChannelRegistered)                                => Console.printLine(s"register Channel: ${ch}")
      case ChannelEvent(ch, ChannelUnregistered)                              => Console.printLine(s"unregister Channel: ${ch}")
      case ChannelEvent(_, ExceptionCaught(cause))                            => Console.printLine(s"Channel error!: ${cause.getMessage}")
      case ChannelEvent(ch, UserEventTriggered(UserEvent.HandshakeComplete))  =>
        connections.update(_ + 1)
        ch.writeAndFlush(WebSocketFrame.text("Greetings!"))
      case ChannelEvent(_, ChannelRead(WebSocketFrame.Close(status, reason))) =>
        connections.update(_ - 1)
        Console.printLine("Closing channel with status: " + status + " and reason: " + reason)
    }

  val protocol = SocketProtocol.default.withSubProtocol(Some("json")) // Setup protocol settings

  val decoder = SocketDecoder.default.withExtensions(allowed = true) // Setup decoder settings

  def app(
    connections: Ref[Int],
    pps: Promise[Nothing, Players],
    players: Ref[Map[String, (WebSocketChannel, Queue[String], Queue[String], String => WsPlayer)]],
    observers: Ref[Map[String, WebSocketChannel]],
    hub: Hub[String]
  ): Http[Any, Nothing, Request, Response] =
    Http.collectZIO[Request] {
      case Method.GET -> !! / "greet" / name => ZIO.succeed(Response.text(s"Greetings ${name}!"))
      case Method.GET -> !! / "skat"         =>
        channelSocket(connections, cmds(pps, players, observers, hub)).toSocketApp
          // .withDecoder(decoder)
          // .withProtocol(protocol)
          .toResponse
    }

  def handleObserver(ch: Channel[WebSocketFrame], text: String): Task[Unit] =
    ch.writeAndFlush(WebSocketFrame.text("You should just listen!"))

  def handlePlayer(
    ch: Channel[WebSocketFrame],
    text: String,
    players: Ref[Map[String, (WebSocketChannel, Queue[String], Queue[String], String => WsPlayer)]]
  ): UIO[Unit] =
    println(s"player received: $text")
    players.get.map(_.get(ch.id).head._2).flatMap(_.offer(text)).unit

  def cmds(
    pps: Promise[Nothing, Players],
    players: Ref[Map[String, (WebSocketChannel, Queue[String], Queue[String], String => WsPlayer)]],
    observers: Ref[Map[String, WebSocketChannel]],
    hub: Hub[String]
  ): Handler[Any, Throwable, (WebSocketChannel, String), Unit] =
    Handler.fromFunctionZIO[(WebSocketChannel, String)] {
      case (ch, "end")      => ch.close()
      case (ch, "player")   =>
        for {
          os  <- observers.get
          ps  <- players.get
          psn <-
            for {
              qs       <- (Queue.bounded[String](2), Queue.bounded[String](2)).mapN((in, out) => (in, out))
              (in, out) = qs
              psm      <- players.updateAndGet(_.updated(ch.id, (ch, in, out, name => new WsPlayer(name, in, out))))
            } yield (psm)
            ((Queue.bounded[String](2), Queue.bounded[String](2))
              .mapN((in, out) => (in, out))
              .flatMap((in, out) =>
                players.updateAndGet(_.updated(ch.id, (ch, in, out, name => new WsPlayer(name, in, out))))
              ))
              .when(ps.size <= 3 && !ps.contains(ch.id) && !os.contains(ch.id))
              .someOrElseZIO(ch.writeAndFlush(WebSocketFrame.text("You are already a Player!")).as(ps))

          _ <- Console.printLine(s"players: ${psn.size} $psn")
          _ <- (players.get
                 .map(m => m.values.toVector)
                 .map(ps => Players(ps(0)._4("Dealer"), ps(1)._4("Listener"), ps(2)._4("Speaker")))
                 .flatMap(pps.succeed))
                 .when(psn.size == 3)
        } yield ()
      case (ch, "observer") =>
        for {
          ps <- players.get
          os <- observers.get
          _  <- observers
                  .update(_.updated(ch.id, ch))
                  .when(!ps.contains(ch.id) && !os.contains(ch.id))
                  .someOrElseZIO(ch.writeAndFlush(WebSocketFrame.text("You are already Player or Observer!")))
        } yield ()
      case (ch, text)       =>
        val hp = handlePlayer(ch, text, players).whenRef(players)(_.contains(ch.id)).map(_._2)
        val ho = handleObserver(ch, text).whenRef(observers)(_.contains(ch.id)).map(_._2)
        val z  = ch.writeAndFlush(WebSocketFrame.text(s"OOPS: received $text!"))
        hp.someOrElseZIO(ho.someOrElseZIO(z))
      // ch.writeAndFlush(WebSocketFrame.text(s"Player: $text")).whenRef(players)(_.contains(ch.id))
      // ch.write(WebSocketFrame.text(text)).repeatN(10) *> ch.flush
    }

  def skat(
    playersRef: Ref[Map[String, (WebSocketChannel, Queue[String], Queue[String], String => WsPlayer)]],
    players: Players,
    hub: Hub[String]
  ): ZIO[Scope, PlayerError, (Player, BiddingValue.Type, Trump, DeclarerPoints)] =
    for {
      psRef     <- playersRef.get
      outFibers <- ZIO.foreach(psRef.values)((ch, _, out, _) =>
                     out.take.flatMap(s => ch.writeAndFlush(WebSocketFrame.text(s))).forever.fork
                   )
      r         <- Skat.game.provideService(players).toZIOWith(FullDeck.apply)

    } yield r

  override val run =
    for {
      connections <- Ref.make(0)
      pps         <- Promise.make[Nothing, Players]
      players     <- Ref.make(Map.empty[String, (WebSocketChannel, Queue[String], Queue[String], String => WsPlayer)])
      observers   <- Ref.make(Map.empty[String, WebSocketChannel])
      hub         <- Hub.bounded[String](10)
      // _           <- hub.subscribe.flatMap(_.take.tap(Console.printLine(_))).forever.fork
      // _           <- (pps.await.flatMap(chs => Console.printLine(s"hallo: $chs").delay(3.second).repeat(Schedule.forever))).fork
      skatFiber   <- (pps.await.flatMap(skat(players, _, hub))).fork
      _           <- Server.serve(app(connections, pps, players, observers, hub)).provide(Server.default)
    } yield ()
