package skat.datatype

enum Announcement(val value:Int):
  case No        extends Announcement(1)
  case Schneider extends Announcement(2)
  case Schwarz   extends Announcement(3)
  case Ouvert    extends Announcement(4)
