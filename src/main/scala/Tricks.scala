package skat.datatype

type PlayerTrickDeck  = Seq[Card] // 0 - 32
case class PlayerTrickDecks(dealer:PlayerTrickDeck, listener: PlayerTrickDeck, speaker: PlayerTrickDeck)

object Tricks:
  def run(ps:Players,pds:PlayerDecks, skat:SkatDeck, t:Trump): PlayerTrickDecks = ???

