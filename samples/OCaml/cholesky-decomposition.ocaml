let cholesky inp =
   let n = Array.length inp in
   let res = Array.make_matrix n n 0.0 in
   let factor i k =
      let rec sum j =
         if j = k then 0.0 else
         res.(i).(j) *. res.(k).(j) +. sum (j+1) in
      inp.(i).(k) -. sum 0 in
   for col = 0 to n-1 do
      res.(col).(col) <- sqrt (factor col col);
      for row = col+1 to n-1 do
         res.(row).(col) <- (factor row col) /. res.(col).(col)
      done
   done;
   res

let pr_vec v = Array.iter (Printf.printf " %9.5f") v; print_newline()
let show = Array.iter pr_vec
let test a =
   print_endline "\nin:"; show a;
   print_endline "out:"; show (cholesky a)

let _ =
   test [| [|25.0; 15.0; -5.0|];
           [|15.0; 18.0;  0.0|];
           [|-5.0;  0.0; 11.0|] |];
   test [| [|18.0; 22.0;  54.0;  42.0|];
           [|22.0; 70.0;  86.0;  62.0|];
           [|54.0; 86.0; 174.0; 134.0|];
           [|42.0; 62.0; 134.0; 106.0|] |];
