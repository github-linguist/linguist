# let gnome_sort a =
    let i = ref 1
    and j = ref 2 in
    while !i < Array.length a do
      if a.(!i-1) <= a.(!i) then
      begin
        i := !j;
        j := !j + 1;
      end else begin
        swap a (!i-1) (!i);
        i := !i - 1;
        if !i = 0 then begin
          i := !j;
          j := !j + 1;
        end;
      end;
    done;
  ;;
val gnome_sort : 'a array -> unit = <fun>

# let a = [| 7; 9; 4; 2; 1; 3; 6; 5; 0; 8; |] ;;
val a : int array = [|7; 9; 4; 2; 1; 3; 6; 5; 0; 8|]

# gnome_sort a ;;
- : unit = ()

# a ;;
- : int array = [|0; 1; 2; 3; 4; 5; 6; 7; 8; 9|]
