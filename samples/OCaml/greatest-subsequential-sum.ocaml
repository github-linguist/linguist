let maxsubseq =
  let rec loop sum seq maxsum maxseq = function
    | [] -> maxsum, List.rev maxseq
    | x::xs ->
        let sum = sum + x
        and seq = x :: seq in
          if sum < 0 then
            loop 0 [] maxsum maxseq xs
          else if sum > maxsum then
            loop sum seq sum seq xs
          else
            loop sum seq maxsum maxseq xs
  in
    loop 0 [] 0 []

let _ =
  maxsubseq [-1 ; -2 ; 3 ; 5 ; 6 ; -2 ; -1 ; 4; -4 ; 2 ; -1]
