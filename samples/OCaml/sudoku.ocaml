(* Ocamlgraph demo program: solving the Sudoku puzzle using graph coloring
   Copyright 2004-2007 Sylvain Conchon, Jean-Christophe Filliatre, Julien Signoles

   This software is free software; you can redistribute it and/or modify
   it under the terms of the GNU Library General Public License version 2,
   with the special exception on linking described in file LICENSE.

   This software is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. *)

open Format
open Graph

(* We use undirected graphs with nodes containing a pair of integers
   (the cell coordinates in 0..8 x 0..8).
   The integer marks of the nodes will store the colors. *)
module G = Imperative.Graph.Abstract(struct type t = int * int end)

(* The Sudoku grid = a graph with 9x9 nodes *)
let g = G.create ()

(* We create the 9x9 nodes, add them to the graph and keep them in a matrix
   for later access *)
let nodes =
  let new_node i j = let v = G.V.create (i, j) in G.add_vertex g v; v in
  Array.init 9 (fun i -> Array.init 9 (new_node i))

let node i j = nodes.(i).(j) (* shortcut for easier access *)

(* We add the edges:
   two nodes are connected whenever they can't have the same value,
   i.e. they belong to the same line, the same column or the same 3x3 group *)
let () =
  for i = 0 to 8 do for j = 0 to 8 do
    for k = 0 to 8 do
      if k <> i then G.add_edge g (node i j) (node k j);
      if k <> j then G.add_edge g (node i j) (node i k);
    done;
    let gi = 3 * (i / 3) and gj = 3 * (j / 3) in
    for di = 0 to 2 do for dj = 0 to 2 do
      let i' = gi + di and j' = gj + dj in
      if i' <> i || j' <> j then G.add_edge g (node i j) (node i' j')
    done done
  done done

(* Displaying the current state of the graph *)
let display () =
  for i = 0 to 8 do
    for j = 0 to 8 do printf "%d" (G.Mark.get (node i j)) done;
    printf "\n";
  done;
  printf "@?"

(* We read the initial constraints from standard input and we display g *)
let () =
  for i = 0 to 8 do
    let s = read_line () in
    for j = 0 to 8 do match s.[j] with
      | '1'..'9' as ch -> G.Mark.set (node i j) (Char.code ch - Char.code '0')
      | _ -> ()
    done
  done;
  display ();
  printf "---------@."

(* We solve the Sudoku by 9-coloring the graph g and we display the solution *)
module C = Coloring.Mark(G)

let () = C.coloring g 9; display ()
