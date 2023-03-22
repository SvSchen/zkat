package skat.datatype

sealed trait Trump { val announcement: Announcement }
case class Suite(suite: Card.Suite.Value, override val announcement: Announcement) extends Trump
case class Grand(override val announcement: Announcement)                          extends Trump
case class Null(override val announcement: Announcement)                           extends Trump

object Trump:
  def selectTrump(declarer:Player,skat:SkatDeck,pds:PlayerDecks):(SkatDeck,PlayerDecks,Trump) = ???
