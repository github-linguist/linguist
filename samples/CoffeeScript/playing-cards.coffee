#translated from JavaScript example
class Card
  constructor: (@pip, @suit) ->

  toString: => "#{@pip}#{@suit}"

class Deck
  pips = '2 3 4 5 6 7 8 9 10 J Q K A'.split ' '
  suits = '♣ ♥ ♠ ♦'.split ' '

  constructor: (@cards) ->
    if not @cards?
      @cards = []
      for suit in suits
        for pip in pips
          @cards.push new Card(pip, suit)

  toString: => "[#{@cards.join(', ')}]"

  shuffle: =>
    for card, i in @cards
      randomCard = parseInt @cards.length * Math.random()
      @cards[i] = @cards.splice(randomCard, 1, card)[0]

  deal: -> @cards.shift()
