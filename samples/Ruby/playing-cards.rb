class Card
    # class constants
    Suits = ["Clubs","Hearts","Spades","Diamonds"]
    Pips = ["2","3","4","5","6","7","8","9","10","Jack","Queen","King","Ace"]

    # class variables (private)
    @@suit_value = Hash[ Suits.each_with_index.to_a ]
    @@pip_value = Hash[ Pips.each_with_index.to_a ]

    attr_reader :pip, :suit

    def initialize(pip,suit)
        @pip = pip
        @suit = suit
    end

    def to_s
        "#{@pip} #{@suit}"
    end

    # allow sorting an array of Cards: first by suit, then by value
    def <=>(card)
        (@@suit_value[@suit] <=> @@suit_value[card.suit]).nonzero? or \
         @@pip_value[@pip] <=> @@pip_value[card.pip]
    end
end

class Deck
    def initialize
        @deck = []
        for suit in Card::Suits
            for pip in Card::Pips
                @deck << Card.new(pip,suit)
            end
        end
    end

    def to_s
        "[#{@deck.join(", ")}]"
    end

    def shuffle!
        @deck.shuffle!
        self
    end

    def deal(*args)
        @deck.shift(*args)
    end
end

deck = Deck.new.shuffle!
puts card = deck.deal
hand = deck.deal(5)
puts hand.join(", ")
puts hand.sort.join(", ")
