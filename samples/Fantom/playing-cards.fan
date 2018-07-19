enum class Suit { clubs, diamonds, hearts, spades }
enum class Pips { ace, two, three, four, five,
  six, seven, eight, nine, ten, jack, queen, king
}

class Card
{
  readonly Suit suit
  readonly Pips pips

  new make (Suit suit, Pips pips)
  {
    this.suit = suit
    this.pips = pips
  }

  override Str toStr ()
  {
    "card: $pips of $suit"
  }
}

class Deck
{
  Card[] deck := [,]

  new make ()
  {
    Suit.vals.each |val|
    {
      Pips.vals.each |pip| { deck.add (Card(val, pip)) }
    }
  }

  Void shuffle (Int swaps := 50)
  {
    swaps.times { deck.swap(Int.random(0..deck.size-1), Int.random(0..deck.size-1)) }
  }

  Card[] deal (Int cards := 1)
  {
    if (cards > deck.size) throw ArgErr("$cards is larger than ${deck.size}")
    Card[] result := [,]
    cards.times { result.push (deck.removeAt (0)) }
    return result
  }

  Void show ()
  {
    deck.each |card| { echo (card.toStr) }
  }
}

class PlayingCards
{
  public static Void main ()
  {
    deck := Deck()
    deck.shuffle
    Card[] hand := deck.deal (7)
    echo ("Dealt hand is:")
    hand.each |card| { echo (card.toStr) }
    echo ("Remaining deck is:")
    deck.show
  }
}
