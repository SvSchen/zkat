package skat.datatype

import zio.prelude.fx.ZPure
import zio.prelude._

sealed trait Trump {
  val announcement: Announcement
  val hand: Boolean
  def sort(cs: Seq[Card]): Seq[Card]
  def sortBy[A](f: A => Card)(cs: Seq[A]): Seq[A]
}

final case class Suite(suite: Card.Suite, override val announcement: Announcement, override val hand: Boolean)
    extends Trump:
  private val ordering = Suite.SuiteCardOrdering(suite)
  def sort(cs: Seq[Card]): Seq[Card]              = cs.sorted(ordering)
  def sortBy[A](f: A => Card)(cs: Seq[A]): Seq[A] = cs.sortBy(f)(ordering)
object Suite:
  class SuiteCardOrdering(t:Card.Suite) extends scala.math.Ordering[Card]:
    def compare(a:Card, b:Card):Int = 
      if a.suite == b.suite then Grand.GrandCardOrdering.compare(a,b)
      else if a.suite == t then 1
      else if b.suite == t then -1
      else Grand.GrandCardOrdering.compare(a,b)

final case class Grand(override val announcement: Announcement, override val hand: Boolean) extends Trump:
  def sort(cs: Seq[Card]): Seq[Card]              = Grand.sort(cs)
  def sortBy[A](f: A => Card)(cs: Seq[A]): Seq[A] = Grand.sortBy(f)(cs)
object Grand:
  val value = 24
  def sort(cs: Seq[Card]): Seq[Card]              = cs.sorted(GrandCardOrdering)
  def sortBy[A](f: A => Card)(cs: Seq[A]): Seq[A] = cs.sortBy(f)(GrandCardOrdering)
  object GrandCardOrdering extends scala.math.Ordering[Card]:
    def compare(a:Card, b:Card):Int = 
      if a.face == b.face then a.suite.ordinal.compare(b.suite.ordinal)
      else if a.face == Card.Face.Jack then 1
      else if b.face == Card.Face.Jack then -1
      else a.face.ordinal.compare(b.face.ordinal)

final case class Null(override val announcement: Announcement, override val hand: Boolean) extends Trump:
  def sort(cs: Seq[Card]): Seq[Card]                      = Null.sort(cs)
  def sortBy[A](f: A => Card)(cs: Seq[A]): Seq[A]         = Null.sortBy(f)(cs)
  def points(declarer: PlayerTrickDeck, bv: BiddingValue) =
    import Announcement.*
    (announcement, hand) match
      case (No | Schneider | Schwarz, false) => 23
      case (No | Schneider | Schwarz, true)  => 35
      case (Ouvert, false)                   => 46
      case (Ouvert, true)                    => 59
object Null:
  val values: NonEmptyList[Int] = NonEmptyList(23,35,46,59)
  def sort(cs: Seq[Card]): Seq[Card]              = cs.sorted(NullCardOrdering)
  def sortBy[A](f: A => Card)(cs: Seq[A]): Seq[A] = cs.sortBy(f)(NullCardOrdering)
  object NullCardOrdering extends scala.math.Ordering[Card]:
    def compare(a:Card, b:Card):Int = 
      if a.face == b.face then a.suite.ordinal.compare(b.suite.ordinal)
      else a.face.ordinal.compare(b.face.ordinal)

