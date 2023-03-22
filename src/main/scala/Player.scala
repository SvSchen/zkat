package skat.datatype

case class Player(name: String) {
  def requestBiddingValue: Option[BiddingValue]         = ???
  def accept(value: BiddingValue): Option[BiddingValue] = ???
  def requestTrump(skat: SkatDeck): (Trump, SkatDeck)   = ???
  def requestCard: Card                                 = ???
}

type Players = (Player, Player, Player)
