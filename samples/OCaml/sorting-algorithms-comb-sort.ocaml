let comb_sort ~input =
  let input_length = Array.length input in
  let gap = ref(input_length) in
  let swapped = ref true in
  while (!gap > 1 || !swapped) do
    if (!gap > 1) then
      gap := int_of_float (float !gap /. 1.3);

    swapped := false;
    for i = 0 to input_length - !gap do
      if input.(i) > input.(i + !gap) then begin
        let tmp = input.(i) in
        input.(i) <- input.(i + !gap);
        input.(i + !gap) <- tmp;
        swapped := true;
      end
    done
  done
;;
