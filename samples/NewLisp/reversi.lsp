#!/usr/bin/env newlisp
;; @module reversi.lsp
;; @description a simple version of Reversi: you as white against newLISP as black
;; @version 0.1 alpha August 2007
;; @author cormullion
;;
;; 2008-10-08 21:46:54
;; updated for newLISP version 10. (changed nth-set to setf)
;; this now does not work with newLISP version 9!
;;
;; This is my first attempt at writing a simple application using newLISP-GS.
;; The game algorithms are basically by 
;; Peter Norvig http://norvig.com/paip/othello.lisp
;; and all I've done is translate to newLISP and add the interface...
;;
;; To-Do: work out how to handle the end of the game properly...
;; To-Do: complete newlispdoc for the functions

(constant 'empty 0) 
(constant 'black 1) 
(constant 'white 2)
(constant 'outer 3) ; squares outside the 8x8 board

(set '*board* '()) ; the master board is a 100 element list
(set '*moves* '()) ; list of moves made

; these are the 8 different directions from a square on the board

(set 'all-directions '(-11 -10 -9 -1 1 9 10 11))

; return a list of all the playable squares (the 8 by 8 grid inside the 10by10

(define (all-squares)
  (local (result)
     (for (square 11 88)
        (if (<= 1 (mod square 10) 8)
           (push square result -1)))
result))

; make a board

(define (make-board)
  (set '*board* (dup outer 100))
  (dolist (s (all-squares))
     (setf (*board* s) empty)))

; for testing and working at a terminal

(define (print-board)
  (print { })
  (for (c 1 8)
     (print c))
  (set 'c 0)
  (for (i 0 99)
     (cond
        ((= (*board* i) 0) (print {.}))
        ((= (*board* i) 1) (print {b}))
        ((= (*board* i) 2) (print {w})))
     (if (and (<= i 88) (= (mod (+ i 1) 10) 0)) ; newline
        (print "\n" (inc c))))
  (println "\n"))

; the initial starting pattern

(define (initial-board)
  (make-board)
  (setf (*board* 44) white)
  (setf (*board* 55) white)
  (setf (*board* 45) black)
  (setf (*board* 54) black))

(define (opponent player)
  (if (= player black) white black))

(define (player-name player)
  (if (= player white) "white" "black"))
  
(define (valid-move? move)
  (and 
     (integer? move)
     (<= 11 move 88)
     (<= 1 (mod move 10) 8)))

(define (empty-square? square)
  (and
     (valid-move? square)
     (= (*board* square) empty)))
     
; test whether a move is legal. The square must be empty
; and it must flip at least one of the opponent's piece

(define (legal-move? move player)
  (and 
     (empty-square? move)
     (exists (fn (dir) (would-flip? move player dir)) all-directions)))

; would this move by player result in any flips in the given direction?
; if so, return the number of the 'opposite' (bracketing) piece's square

(define (would-flip? move player dir) 
  (let 
     ((c (+ move dir)))
     (and 
        (= (*board* c) (opponent player))
        (find-bracketing-piece (+ c dir) player dir))))
  
(define (find-bracketing-piece square player dir)
  ; return the square of the bracketing piece, if any
  (cond
     ((= (*board* square) player) square)
     ((= (*board* square) (opponent player))
        (find-bracketing-piece (+ square dir) player dir))
     (true nil)))

(define (make-flips move player dir)
  (let 
     ((bracketer (would-flip? move player dir))
      (c (+ move dir)))
  (if bracketer
     (do-until (= c bracketer)
        (setf (*board* c) player)
        (push c *flips* -1)
        (inc c dir)))))

; make the move on the master game board, not yet visually

(define (make-move move player)
  (setf (*board* move) player)
  (push move *moves* -1)
  (set '*flips* '()) ; we're going to keep a record of the flips made
  (dolist (dir all-directions)
     (make-flips move player dir)))

(define (next-to-play previous-player)
  (let ((opp (opponent previous-player)))
     (cond
        ((any-legal-move? opp) opp)
        ((any-legal-move? previous-player)
           (println (player-name opp) " has no moves")
           previous-player)
        (true nil))))
        
; are there any legal moves (returns first) for this player?
(define (any-legal-move? player)
  (exists (fn (move) (legal-move? move player)) 
     (all-squares)))

; a list of all legal moves might be useful
(define (legal-moves player)
  (let ((result '()))
     (dolist (move (all-squares))
        (if (legal-move? move player)
           (push move result)))
  (unique result)))

; define any number of strategies that can be called on to calculate
; the next computer move. This is the only one I've done... - make 
; any legal move at random!

(define (random-strategy player)
  (seed (date-value))
  (apply amb (legal-moves player)))

; get the next move using a particular strategy

(define (get-move strategy player)
 (let ((move (apply strategy (list player))))
  (cond
     ((and
        (valid-move? move)
        (legal-move? move player))
            (make-move move player))
     (true  
        (println "no valid or legal move for " (player-name player) )
        nil))
  move))

; that's about all the game algorithms for now
; now for the interface

(if (= ostype "Win32")
   (load (string (env "PROGRAMFILES") "/newlisp/guiserver.lsp"))
   (load "/usr/share/newlisp/guiserver.lsp")
)

(gs:init)
(map set '(screen-width screen-height) (gs:get-screen))
(set 'board-width 540)
; center on screen
(gs:frame 'Reversi (- (/ screen-width 2) (/ board-width 2)) 60 board-width 660 "Reversi")
(gs:set-border-layout 'Reversi)

(gs:canvas 'MyCanvas 'Reversi)
  (gs:set-background 'MyCanvas '(.8 .9 .7 .8))
  (gs:mouse-released 'MyCanvas 'mouse-released-action true)

(gs:panel 'Controls)
  (gs:button 'Start 'start-game "Start")

(gs:panel 'Lower)
  (gs:label 'WhiteScore "")
  (gs:label 'BlackScore "")

(gs:add-to 'Controls 'Start )
(gs:add-to 'Lower 'WhiteScore 'BlackScore)
(gs:add-to 'Reversi 'MyCanvas "center" 'Controls "north" 'Lower "south")

(gs:set-anti-aliasing true)
(gs:set-visible 'Reversi true)

; size of board square, and radius/width of counter
(set 'size 60 'width 30)

; initialize the master board

(define (initial-board)
  (make-board)
  (setf (*board* 44) white)
  (setf (*board* 55) white)
  (setf (*board* 45) black)
  (setf (*board* 54) black)  
)

; draw a graphical repesentation of the board

(define (draw-board)
  (local (x y)
     (dolist (i (all-squares))
        (map set '(x y) (square-to-xy i))
        (gs:draw-rect 
           (string x y) 
           (- (* y size) width ) ; !!!!!!
           (- (* x size) width )
           (* width 2)
           (* width 2)
           gs:white))))

(define (draw-first-four-pieces)
  (draw-piece 44 "white")
  (draw-piece 55 "white")
  (draw-piece 45 "black")
  (draw-piece 54 "black"))

; this next function can mark the legal moves available to a player

(define (show-legal-moves player)
  (local (legal-move-list x y)
     (set 'legal-move-list (legal-moves player))
     (dolist (m (all-squares))
        (map set '(x y) (square-to-xy m))
        (gs:draw-rect 
           (string x y) 
           (- (* y size) width ) ; !!!!!!
           (- (* x size) width )
           (* width 2)
           (* width 2)
           (if (find m legal-move-list) gs:blue gs:white)
        )
     )
  )
)

; convert the number of a square on the master board to coordinates

(define (square-to-xy square) 
  (list (/ square 10) (mod square 10)))

; draw one of the pieces

(define (draw-piece square colour)
  (local (x y)
  (map set '(x y) (square-to-xy square))
  (cond 
     ((= colour "white") 
        (gs:fill-circle 
           (string x y) 
           (* y size)  ; !!!!!!! y first, cos y is x ;-)
           (* x size) 
           width
           gs:white))
     
     ((= colour "black") 
        (gs:fill-circle 
           (string x y) 
           (* y size) 
           (* x size) 
           width
           gs:black))
     
     ((= colour "empty") 
        (gs:draw-rect 
           (string x y) 
           (- (* y size) width ) 
           (- (* x size) width )
           (* width 2)
           (* width 2)
           gs:white))
  )))

; animate the pieces flipping

(define (flip-piece square player)
; flip by drawing thinner and fatter ellipses 
; go from full disk in opposite colour to invisible
; then from invisible to full disk in true colour
  (local (x y colour)
     (map set '(x y) (square-to-xy square))
     ; delete original piece
     (gs:delete-tag (string x y))
     (set 'colour (if (= player 2) gs:black gs:white )) 
     (for (i width  1 -3)
        (gs:fill-ellipse 
           (string x y {flip} i) 
           (* y size) ; y first :-) !!! 
           (* x size) 
           i 
           width
           colour)
        (sleep 20)  ; this might need adjusting...
        (gs:delete-tag (string x y {flip} i))
     )
     (set 'colour (if (= player 2) gs:white gs:black))
     (for (i 1 width 3)
        (gs:fill-ellipse 
           (string x y {flip} i) 
           (* y size) ; :-) !!! 
           (* x size) 
           i 
           width
           colour)
        (sleep 20)  
        (gs:delete-tag (string x y {flip} i))
     )
     ; draw the piece again
     (gs:fill-circle 
           (string x y) 
           (* y size)
           (* x size) 
           width
           colour)
  )
)

(define (do-move move player)
  (cond 
     ; check if the move is good ...
     ((and (!= player nil)
           (valid-move? move)
           (legal-move? move player))
           
           ; ... play it
              ; make move on board
              (make-move move player)
              ; and on screen
              (draw-piece move (player-name player))
              (gs:update)
              ; do flipping stuff
              
              ; wait for a while
              (sleep 1000)
  
              ; then do flipping
              (dolist (f *flips*)
                 (flip-piece f player))
              
              (inc *move-number*)
              (draw-piece move (player-name player))
              (gs:update)

              ; update scores
              (gs:set-text 'WhiteScore 
                 (string "White: " (first (count (list white) *board*))))
              (gs:set-text 'BlackScore
                 (string "Black: " (first (count (list black) *board*))))
              )
     ; or return nil
     (true 
           nil)))

; the game is driven by the mouse clicks of the user
; in reply, the computer plays a black piece
; premature clicking is possible and possibly a bad thing...

(define (mouse-released-action x y button modifiers tags)
  ; extract the tag of the clicked square
  (set 'move (int (string (first tags)) 0 10))
  (if (do-move move player)
     (begin
        (set 'player (next-to-play player))
        ; there is a training mode - legal squares are highlighted
        ; you can uncomment the next line...
        ; (show-legal-moves player)
        (gs:update)
        
        ; wait for black's reply
        (gs:set-cursor 'Reversi "wait")
        (gs:set-text 'Start "black's move - thinking...")
        ; give the illusion of Deep Thought...
        (sleep 2000)
        ; black's reply
        ; currently only the random strategy has been defined...
        (set 'strategy random-strategy)
        (set 'move (apply strategy (list player)))
        (do-move move player)
        (set 'player (next-to-play player))
        ; (show-legal-moves player) ; to see black's moves
        (gs:set-text 'Start "your move")
        (gs:set-cursor 'Reversi "default")
        (gs:update))))

(define (start-game)
  (gs:set-text 'Start "Click a square to place a piece!")
  (gs:disable 'Start)
  (set 'player white))

(define (start)
  (gs:set-text 'Start "Start")
  (gs:enable 'Start)
  (set  '*move-number* 1
        '*flips* '())
  (initial-board)
  (draw-board)
  (draw-first-four-pieces))

(start)

(gs:listen)