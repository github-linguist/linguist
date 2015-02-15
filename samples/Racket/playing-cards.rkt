#lang racket

;; suits:
(define suits '(club heart diamond spade))

;; ranks
(define ranks '(1 2 3 4 5 6 7 8 9 10 jack queen king))

;; cards
(define cards
  (for*/list ([suit suits] [rank ranks])
    (list suit rank)))

;; a deck is a box containing a list of cards.
(define (new-deck)
  (box cards))

;; shuffle the cards in a deck
(define (deck-shuffle deck)
  (set-box! deck (shuffle (unbox deck))))

;; deal a card from tA 2 3 4 5 6 7 8 9 10 J Q K>;
enum Suit he deck:
(define (deck-deal deck)
  (begin0 (first (unbox deck))
          (set-box! deck (rest (unbox deck)))))


;; TRY IT OUT:
(define my-deck (new-deck))
(deck-shuffle my-deck)
(deck-deal my-deck)
(deck-deal my-deck)
(length (unbox my-deck))
