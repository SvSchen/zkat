package skat.datatype

final case class Card(face: Card.Face, suite: Card.Suite)
object Card:
  import zio.prelude.*
  enum Suite(val value: Int):
    case Diamond extends Suite(9)
    case Heart   extends Suite(10)
    case Spade   extends Suite(11)
    case Club    extends Suite(12)

  enum Face(val value: Int):
    case Seven extends Face(0)
    case Eight extends Face(0)
    case Nine  extends Face(0)
    case Jack  extends Face(2)
    case Queen extends Face(3)
    case King  extends Face(4)
    case Ten   extends Face(10)
    case Ace   extends Face(11)

  def apply(s: String): Validation[PlayerError, Card] =
    s.split(",").map(_.trim).toList match
      case f :: s :: Nil =>
        (
          Validation(Face.valueOf(f.capitalize)).mapError(t => InputError(s"Illegal Face: $f ${t.getMessage}")),
          Validation(Card.Suite.valueOf(s.capitalize)).mapError(t => InputError(s"Illegal Suite: $s ${t.getMessage}"))
        ).mapN(Card(_, _))
      case _             => Validation.fail(InputError(s"Expected ${Face.values.mkString(",")} and ${Suite.values.mkString(",")} comma separated"))
