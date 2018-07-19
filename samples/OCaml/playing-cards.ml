type pip = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten |
           Jack | Queen | King | Ace
let pips = [Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten;
            Jack; Queen; King; Ace]

type suit = Diamonds | Spades | Hearts | Clubs
let suits = [Diamonds; Spades; Hearts; Clubs]

type card = pip * suit

let full_deck = Array.of_list (List.concat (List.map (fun pip -> List.map (fun suit -> (pip, suit)) suits) pips))

(* Fisher-Yates shuffle *)
let shuffle deck =
  for i = Array.length deck - 1 downto 1 do
    let j = Random.int (i+1) in
    (* swap deck.(i) and deck.(j) *)
    let temp = deck.(i) in
    deck.(i) <- deck.(j);
    deck.(j) <- temp
  done
