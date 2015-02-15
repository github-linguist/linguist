require random.fs   \ RANDOM ( n -- 0..n-1 ) is called CHOOSE in other Forths

create pips  s" A23456789TJQK" mem,
create suits s" DHCS"          mem, \ diamonds, hearts, clubs, spades
: .card ( c -- )
  13 /mod swap
  pips  + c@ emit
  suits + c@ emit ;

create deck 52 allot
variable dealt

: new-deck
  52 0        do i deck i + c!             loop  0 dealt ! ;
: .deck
  52 dealt @ ?do   deck i + c@ .card space loop  cr ;
: shuffle
  51 0 do
    52 i - random i + ( rand-index ) deck +
    deck i + c@  over c@
    deck i + c!  swap c!
  loop ;
: cards-left ( -- n ) 52 dealt @ - ;
: deal-card ( -- c )
  cards-left 0= abort" Deck empty!"
  deck dealt @ + c@  1 dealt +! ;
: .hand ( n -- )
  0 do deal-card .card space loop cr ;

new-deck shuffle .deck
5 .hand
cards-left .  \ 47
