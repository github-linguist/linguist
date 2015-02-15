#load "str.cma"

let txt = "In olden times when wishing still helped one, there lived
a king whose daughters were all beautiful, but the youngest was so
beautiful that the sun itself, which has seen so much, was astonished
whenever it shone in her face.  Close by the king's castle lay a great
dark forest, and under an old lime-tree in the forest was a well, and
when the day was very warm, the king's child went out into the forest
and sat down by the side of the cool fountain, and when she was bored
she took a golden ball, and threw it up on high and caught it, and
this ball was her favorite plaything."

let () =
  let line_width = int_of_string Sys.argv.(1) in
  let words = Str.split (Str.regexp "[ \n]+") txt in
  let buf = Buffer.create 10 in
  let _ =
    List.fold_left (fun (width, sep) word ->
      let wlen = String.length word in
      let len = width + wlen + 1 in
      if len > line_width then
      begin
        Buffer.add_char buf '\n';
        Buffer.add_string buf word;
        (wlen, " ")
      end else begin
        Buffer.add_string buf sep;
        Buffer.add_string buf word;
        (len, " ")
      end
    ) (0, "") words
  in
  print_endline (Buffer.contents buf)
