package skat.datatype

case class Card(kind: Card.Kind.Value, suite: Card.Suite.Value, color: Card.Color.Value)
object Card {
  object Color extends Enumeration {
    type Color = Value
    val Red, Black = Value
  }

  object Suite extends Enumeration {
    type Suite = Value
    val Diamond, Heart, Spade, Club = Value
  }

  object Kind extends Enumeration {
    import scala.language.implicitConversions
    protected case class KindVal(value: Int) extends super.Val
    implicit def valueToKindVal(v: Value): KindVal = v.asInstanceOf[KindVal]
    val Seven                                      = KindVal(0)
    val Eight                                      = KindVal(0)
    val Nine                                       = KindVal(0)
    val Jack                                       = KindVal(2)
    val Queen                                      = KindVal(3)
    val King                                       = KindVal(4)
    val Ten                                        = KindVal(10)
    val Ace                                        = KindVal(11)
  }
}
