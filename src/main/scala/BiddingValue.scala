package skat.datatype

case class BiddingValue(value: Int) extends AnyVal
object BiddingValue:
  def min = ???

object Bidding:
  def run(ps:Players,pds:PlayerDecks): (Player, BiddingValue) = ???
