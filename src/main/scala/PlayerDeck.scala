package skat.datatype

type PlayerDeck  = Seq[Card] //10
// type PlayerDecks = (PlayerDeck, PlayerDeck, PlayerDeck)
case class PlayerDecks(dealer:PlayerDeck, listener: PlayerDeck, speaker: PlayerDeck)
