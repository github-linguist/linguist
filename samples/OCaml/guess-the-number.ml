#!/usr/bin/env ocaml

let () =
  Random.self_init();
  let n =
    if Random.bool () then
      let n = 2 + Random.int 8 in
      print_endline "Please guess a number between 1 and 10 excluded";
      (n)
    else
      let n = 1 + Random.int 10 in
      print_endline "Please guess a number between 1 and 10 included";
      (n)
  in
  while read_int () <> n do
    print_endline "The guess was wrong! Please try again!"
  done;
  print_endline "Well guessed!"
