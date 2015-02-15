type dir = U | D | L | R
type move_t = Move of dir | Push of dir

let letter = function
   | Push(U) -> 'U' | Push(D) -> 'D' | Push(L) -> 'L' | Push(R) -> 'R'
   | Move(U) -> 'u' | Move(D) -> 'd' | Move(L) -> 'l' | Move(R) -> 'r'

let cols = ref 0
let delta = function U -> -(!cols) | D -> !cols | L -> -1 | R -> 1

let store = Hashtbl.create 251
let mark t = Hashtbl.replace store t ()
let marked t = Hashtbl.mem store t

let show ml =
   List.iter (fun c -> print_char (letter c)) (List.rev ml); print_newline()

let gen_moves (x,boxes) bd =
   let empty i = bd.(i) = ' ' && not (List.mem i boxes) in
   let check l dir =
      let dx = delta dir in
      let x1 = x+dx in
      if List.mem x1 boxes then (
         if empty (x1+dx) then Push(dir) :: l else l
      ) else (
         if bd.(x1) = ' ' then Move(dir) :: l else l
      ) in
   (List.fold_left check [] [U; L; R; D])

let do_move (x,boxes) = function
   | Push(d) -> let dx = delta d in
      let x1 = x+dx in let x2 = x1+dx in
      let rec shift = function
         | [] -> failwith "shift"
         | h :: t -> if h = x1 then x2 :: t else h :: shift t in
      x1, List.fast_sort compare (shift boxes)
   | Move(d) -> (x+(delta d)), boxes

let init_pos bd =
   let p = ref 0 in
   let q = ref [] in
   let check i c =
      if c = '$' || c = '*' then q := i::!q
      else if c = '@' then p := i in (
   Array.iteri check bd;
   (!p, List.fast_sort compare !q);
   )

let final_box bd =
   let check (i,l) c = if c = '.' || c = '*' then (i+1,i::l) else (i+1,l) in
   List.fast_sort compare (snd (Array.fold_left check (0,[]) bd))

let array_of_input inp =
   let r = List.length inp and c = String.length (List.hd inp) in
   let a = Array.create (r*c) ' ' in (
   for i = 0 to pred r do
      let s = List.nth inp i in
      for j = 0 to pred c do a.(i*c+j) <- s.[j] done
   done;
   cols := c; a)

let solve b =
   let board = array_of_input b in
   let targets = final_box board in
   let solved pos = targets = snd pos in
   let clear = Array.map (function '#' -> '#' | _ -> ' ') in
   let bdc = clear board in
   let q = Queue.create () in
   let pos1 = init_pos board in
   begin
      mark pos1;
      Queue.add (pos1, []) q;
      while not (Queue.is_empty q) do
         let curr, mhist = Queue.pop q in
         let moves = gen_moves curr bdc in
         let check m =
            let next = do_move curr m in
            if not (marked next) then
            if solved next then (show (m::mhist); exit 0)
            else (mark next; Queue.add (next,m::mhist) q) in
         List.iter check moves
      done;
      print_endline "No solution"
   end;;

let level = ["#######";
             "#     #";
             "#     #";
             "#. #  #";
             "#. $$ #";
             "#.$$  #";
             "#.#  @#";
             "#######"] in
solve level
