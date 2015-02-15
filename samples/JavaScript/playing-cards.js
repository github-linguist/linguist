function Card(pip, suit) {
    this.pip = pip;
    this.suit = suit;

    this.toString = function () {
        return this.pip + ' ' + this.suit;
    };
}

function Deck() {
    var pips = '2 3 4 5 6 7 8 9 10 Jack Queen King Ace'.split(' ');
    var suits = 'Clubs Hearts Spades Diamonds'.split(' ');
    this.deck = [];
    for (var i = 0; i < suits.length; i++)
        for (var j = 0; j < pips.length; j++)
            this.deck.push(new Card(pips[j], suits[i]));

    this.toString = function () {
        return '[' + this.deck.join(', ') + ']';
    };

    this.shuffle = function () {
        for (var i = 0; i < this.deck.length; i++)
            this.deck[i] = this.deck.splice(
                parseInt(this.deck.length * Math.random()), 1, this.deck[i])[0];
    };

    this.deal = function () {
        return this.deck.shift();
    };
}
