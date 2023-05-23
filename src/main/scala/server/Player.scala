package ws
import zio.http.socket.WebSocketChannel
import zio.prelude.fx.ZPure
import skat.datatype.*
import zio.Enqueue
import zio.Dequeue
import zio.Schedule
import zio.Unsafe
import zio.Exit.Success
import zio.Exit.Failure
import zio.ZIO
import zio.Cause
import scala.deriving.Mirror
import zio.*

class WsPlayer(name: String, in: Dequeue[String], out: Enqueue[String]) extends skat.datatype.Player(name):

  private val rt                               = zio.Runtime.default
  private def print(msg: String): UIO[Boolean] =
    out.offer(msg)

  private def answer(msg: String): ZIO[Any, Nothing, String] =
    println(s"$name: $msg")
    print(msg) *> in.take

  private def error(msg: String): UIO[Boolean] =
    print(s"Error: $msg")

  private def fromZIO[E, A](z: => ZIO[Any, E, A]): ZPure[Nothing, Any, Any, Any, Cause[E], A] =
    ZPure.fromEither(Unsafe.unsafe { implicit unsafe => rt.unsafe.run(z) } match
      case Success(value) => Right(value)
      case Failure(cause) => Left(cause)
    )

  def requestBiddingValue(value: BiddingValue): ZPure[Nothing, Any, Any, Any, Option[PlayerError], BiddingValue] =
    val a = answer(s"Bidding value: $value or higher?")
      .flatMap(_.toLowerCase.trim match {
        case "yes" => ZIO.succeed(value)
        case "no"  => ZIO.fail(Option.empty[PlayerError])
        case s     =>
          ZIO
            .attempt(s.toInt)
            .mapError(t => Some(InputError(s"$s should be an number or either yes or no (${t.getMessage})")))
            .filterOrFail(BiddingValue.values.contains)(Some(InputError(s"$s should be a valid bidding value")))
      })
      .map(BiddingValue(_))
    fromZIO(
      a.tapError(_ match
        case Some(e) => error(e.toString).unit
        case None    => ZIO.unit
      ).retry(Schedule.recurWhile[Option[PlayerError]](_.isDefined) && Schedule.recurs(3))
    ).mapError(_.failures.headOption.flatten)

  def accept(value: BiddingValue): ZPure[Nothing, Any, Any, Any, Option[PlayerError], BiddingValue] =
    val a = answer(s"Accept bidding value: $value?")
      .flatMap(_.toLowerCase.trim match {
        case "yes" => ZIO.succeed(value)
        case "no"  => ZIO.fail(Option.empty[PlayerError])
        case s     => ZIO.fail(Some(InputError(s"$s should be either yes or no")))
      })
    fromZIO(
      a.tapError(_ match
        case Some(e) => error(e.toString).unit
        case None    => ZIO.unit
      ).retry(Schedule.recurWhile[Option[PlayerError]](_.isDefined) && Schedule.recurs(3))
    ).mapError(_.failures.headOption.flatten)

  def requestTrump(skat: SkatDeck): ZPure[Nothing, Any, Nothing, Any, PlayerError, (Trump, SkatDeck)] =
    lazy val suites       = Card.Suite.values.map(_.productPrefix.toLowerCase)
    lazy val announcement = Announcement.values.map(_.productPrefix.toLowerCase)
    val t                 = for {
      h            <- answer(s"Hand?")
      hand         <- h.toLowerCase.trim match {
                        case "yes" => ZIO.succeed(true)
                        case "no"  => ZIO.succeed(false)
                        case s     => ZIO.fail(InputError(s"$s should be either yes or no"))
                      }
      a            <- answer(s"Any announcement?")
      announcement <- ZIO
                        .attempt(Announcement.valueOf(a.toLowerCase.capitalize.trim))
                        .mapError(t => InputError(s"$a should be one of ${Announcement.values.mkString(", ")}"))
      newSkat      <- if hand then ZIO.succeed(skat)
                      else answer(s"Skat: $skat").flatMap(s => ZIO.foreach(s.split("::"))(s => Card(s.trim).toZIO))
      t            <- answer(s"Trump?")
      trump        <- t.toLowerCase.trim match {
                        case "grand"                 => ZIO.succeed(Grand(announcement, hand))
                        case "null"                  => ZIO.succeed(Null(announcement, hand))
                        case s if suites.contains(s) =>
                          ZIO.succeed(Suite(Card.Suite.valueOf(s.capitalize), announcement, hand))
                        case s                       => ZIO.fail(InputError(s"$s should be one of ${suites.mkString(", ")}, Null or Grand"))
                      }
    } yield (trump, newSkat)
    // TODO get rid of asInstanceOf
    fromZIO(
      t.tapError(e => error(e.toString))
        .retry(Schedule.recurs(3))
    )
      .mapError(_.failures.head)
      .asInstanceOf[ZPure[Nothing, Any, Nothing, Any, PlayerError, (Trump, SkatDeck)]]

  def requestTrick(pcd: PlayerCardDeck, t: Trump): ZPure[Nothing, Any, Any, Any, PlayerError, Card] =
    val c = for {
      c    <- answer(s"Your card please, trump: $t trick: ${pcd.mkString(", ")}!")
      card <- Card(c).toZIOParallelErrors
    } yield card
    fromZIO(
      c.tapError(e => error(e.toString))
        .retry(Schedule.recurs(3))
    ).mapError(_.failures.flatten.head)

  def set(pd: PlayerDeck): ZPure[Nothing, Any, Any, Any, Nothing, Unit] =
    println(s"Your deck: ${pd.mkString(", ")}")
    fromZIO(out.offer(s"Your deck: ${pd.mkString(", ")}").unit).mapError(_.failures.head)
