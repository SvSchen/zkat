package skat.datatype

sealed trait Announcement
case object No        extends Announcement
case object Hand      extends Announcement
case object Schneider extends Announcement
case object Schwarz   extends Announcement
case object Ouvert    extends Announcement
