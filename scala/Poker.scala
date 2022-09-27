object Poker {

  def hand(holeCards: List[String], communityCards: List[String]): (String, List[String]) = {
    val cards = (holeCards ++ communityCards).map(Card(_))
    val hand = cards.combinations(5).map(Hand(_)).max
    (hand.name, hand.cards.map(Card.show))
  }

  private val nameList = List("nothing", "pair", "two pair", "three-of-a-kind", "straight", "flush", "full house", "four-of-a-kind", "straight-flush")

  object Hand {
    def apply(_cards: List[Card]): Hand = {

      val sorted = _cards.sortBy(_.rank)
      val groups = sorted.groupBy(_.rank).values.toList.sortBy(_.size).reverse

      val (name, cards) = groups match {
        case a :: b :: Nil if a.size == 4 => ("four-of-a-kind", a.take(1) ++ b)
        case a :: b :: Nil if a.size == 3 && b.size == 2 => ("full house", a.take(1) ++ b.take(1))
        case a :: b :: c :: Nil if a.size == 3 => ("three-of-a-kind", a.take(1) ++ (b ++ c).sortBy(-_.rank))
        case a :: b :: c :: Nil if a.size == 2 && b.size == 2 => ("two pair", (a.take(1) ++ b.take(1)).sortBy(-_.rank) ++ c)
        case a :: b :: c :: d :: Nil if a.size == 2 => ("pair", a.take(1) ++ (b ++ c ++ d).sortBy(-_.rank))
        case _ =>
          val flush = sorted.groupBy(_.suit).size == 1
          val straight = sorted.sliding(2).forall { case List(a, b) => a.rank + 1 == b.rank }

          if (flush && straight) ("straight-flush", sorted.reverse)
          else if (flush) ("flush", sorted.reverse)
          else if (straight) ("straight", sorted.reverse)
          else ("nothing", sorted.reverse)
      }

      new Hand(name, cards)

    }
  }

  case class Hand(name: String, cards: List[Card]) extends Ordered[Hand] {
    def compare(that: Hand): Int = {
      val names = nameList
      val nameOrder = names.indexOf(this.name) - names.indexOf(that.name)
      if (nameOrder != 0) nameOrder
      else {
        this.cards.zip(that.cards).map { case (a, b) => a.rank - b.rank }.dropWhile(_ == 0).headOption.getOrElse(0)
      }
    }
  }

  object Card {
    def apply(s: String): Card = {
      require(s.length == 2 || s.length == 3, s"invalid length of card string: ${s}, length: ${s.length}")
      val rank = s.head match {
        case 'A' => 14
        case 'K' => 13
        case 'Q' => 12
        case 'J' => 11
        case c =>
          if (c == '1') 10
          else c.asDigit
      }
      val suit = s.last match {
        case '♠' => Spades
        case '♣' => Clubs
        case '♥' => Hearts
        case '♦' => Diamonds
      }
      Card(rank, suit)
    }

    def show(c: Card): String = {
      val rank = c.rank match {
        case 14 => "A"
        case 13 => "K"
        case 12 => "Q"
        case 11 => "J"
        case r => r.toString
      }
      rank
    }
  }

  case class Card(rank: Int, suit: Suit)

  sealed trait Suit

  case object Spades extends Suit

  case object Clubs extends Suit

  case object Hearts extends Suit

  case object Diamonds extends Suit

}