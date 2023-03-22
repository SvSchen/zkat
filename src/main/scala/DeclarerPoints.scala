package skat.datatype

case class DeclarerPoints(value: Int) extends AnyVal

object Result:
  def calc(ptds:PlayerTrickDecks, declarer:Player, bv:BiddingValue, t:Trump): DeclarerPoints = ???
