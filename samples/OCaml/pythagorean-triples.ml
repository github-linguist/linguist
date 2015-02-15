let isqrt n =
   let rec iter t =
      let d = n - t*t in
      if (0 <= d) && (d < t+t+1) (* t*t <= n < (t+1)*(t+1) *)
      then t else iter ((t+(n/t))/2)
   in iter 1

let rec gcd a b =
   let t = a mod b in
   if t = 0 then b else gcd b t

let coprime a b = gcd a b = 1

let num_to ms =
   let ctr = ref 0 in
   let prim_ctr = ref 0 in
   let max_m = isqrt (ms/2) in
   for m = 2 to max_m do
      for j = 0 to (m/2) - 1 do
         let n = m-(2*j+1) in
         if coprime m n then
            let s = 2*m*(m+n) in
            if s <= ms then
               (ctr := !ctr + (ms/s); incr prim_ctr)
      done
   done;
  (!ctr, !prim_ctr)

let show i =
  let s, p = num_to i in
  Printf.printf "For perimeters up to %d there are %d total and %d primitive\n%!" i s p;;

List.iter show [ 100; 1000; 10000; 100000; 1000000; 10000000; 100000000 ]
