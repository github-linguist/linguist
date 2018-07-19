(defrecord Card [pip suit]
  Object
  (toString [this] (str pip " of " suit)))

(defprotocol pDeck
  (deal [this n])
  (shuffle [this])
  (newDeck [this])
  (print [this]))

(deftype Deck [cards]
  pDeck
  (deal [this n] [(take n cards) (Deck. (drop n cards))])
  (shuffle [this] (Deck. (shuffle cards)))
  (newDeck [this] (Deck. (for [suit ["Clubs" "Hearts" "Spades" "Diamonds"]
			       pip ["2" "3" "4" "5" "6" "7" "8" "9" "10" "Jack" "Queen" "King" "Ace"]]
			   (Card. pip suit))))
  (print [this] (dorun (map (comp println str) cards)) this))

(defn new-deck []
  (.newDeck (Deck. nil)))
