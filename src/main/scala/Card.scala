package skat.datatype

final case class Card(face: Card.Face, suite: Card.Suite)
object Card:
  enum Suite(val value:Int):
    case Diamond extends Suite(9)
    case Heart extends Suite(10)
    case Spade extends Suite(11)
    case Club extends Suite(12)

  enum Face(val value:Int):
    case Seven extends Face(0)
    case Eight extends Face(0)
    case Nine  extends Face(0)
    case Jack  extends Face(2)
    case Queen extends Face(3)
    case King  extends Face(4)
    case Ten   extends Face(10)
    case Ace   extends Face(11)
