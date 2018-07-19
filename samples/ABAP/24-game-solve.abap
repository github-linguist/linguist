data: lv_flag type c,
      lv_number type i,
      lt_numbers type table of i.

constants: c_no_val type i value 9999.

append 1 to lt_numbers.
append 1 to lt_numbers.
append 2 to lt_numbers.
append 7 to lt_numbers.

write 'Evaluating 24 with the following input: '.
loop at lt_numbers into lv_number.
  write lv_number.
endloop.
perform solve_24 using lt_numbers.

form eval_formula using iv_eval type string changing ev_out type i.
  call function 'EVAL_FORMULA' "analysis of a syntactically correct formula
    exporting
      formula = iv_eval
    importing
      value   = ev_out
    exceptions
   others     = 1.

  if sy-subrc <> 0.
    ev_out = -1.
  endif.
endform.

" Solve a 24 puzzle.
form solve_24 using it_numbers like lt_numbers.
  data: lv_flag   type c,
        lv_op1    type c,
        lv_op2    type c,
        lv_op3    type c,
        lv_var1   type c,
        lv_var2   type c,
        lv_var3   type c,
        lv_var4   type c,
        lv_eval   type string,
        lv_result type i,
        lv_var     type i.

  define retrieve_var.
    read table it_numbers index &1 into lv_var.
    &2 = lv_var.
  end-of-definition.

  define retrieve_val.
    perform eval_formula using lv_eval changing lv_result.
    if lv_result = 24.
        write / lv_eval.
    endif.
  end-of-definition.
  " Loop through all the possible number permutations.
  do.
    " Init. the operations table.

    retrieve_var: 1 lv_var1, 2 lv_var2, 3 lv_var3, 4 lv_var4.
    do 4 times.
      case sy-index.
        when 1.
          lv_op1 = '+'.
        when 2.
          lv_op1 = '*'.
        when 3.
          lv_op1 = '-'.
        when 4.
          lv_op1 = '/'.
      endcase.
      do 4 times.
        case sy-index.
        when 1.
          lv_op2 = '+'.
        when 2.
          lv_op2 = '*'.
        when 3.
          lv_op2 = '-'.
        when 4.
          lv_op2 = '/'.
        endcase.
        do 4 times.
          case sy-index.
          when 1.
            lv_op3 = '+'.
          when 2.
            lv_op3 = '*'.
          when 3.
            lv_op3 = '-'.
          when 4.
            lv_op3 = '/'.
          endcase.
          concatenate '(' '(' lv_var1 lv_op1 lv_var2 ')' lv_op2 lv_var3 ')' lv_op3 lv_var4  into lv_eval separated by space.
          retrieve_val.
          concatenate '(' lv_var1 lv_op1 lv_var2 ')' lv_op2 '(' lv_var3 lv_op3 lv_var4 ')'  into lv_eval separated by space.
          retrieve_val.
          concatenate '(' lv_var1 lv_op1 '(' lv_var2 lv_op2 lv_var3 ')' ')' lv_op3 lv_var4  into lv_eval separated by space.
          retrieve_val.
          concatenate lv_var1 lv_op1 '(' '(' lv_var2 lv_op2 lv_var3 ')' lv_op3 lv_var4 ')'  into lv_eval separated by space.
          retrieve_val.
          concatenate lv_var1 lv_op1 '(' lv_var2 lv_op2 '(' lv_var3 lv_op3 lv_var4 ')' ')'  into lv_eval separated by space.
          retrieve_val.
        enddo.
      enddo.
    enddo.

    " Once we've reached the last permutation -> Exit.
    perform permute using it_numbers changing lv_flag.
    if lv_flag = 'X'.
      exit.
    endif.
  enddo.
endform.


" Permutation function - this is used to permute:
" A = {A1...AN} -> Set of supplied variables.
" B = {B1...BN - 1} -> Set of operators.
" Can be used for an unbounded size set. Relies
" on lexicographic ordering of the set.
form permute using iv_set like lt_numbers
             changing ev_last type c.
  data: lv_len     type i,
        lv_first   type i,
        lv_third   type i,
        lv_count   type i,
        lv_temp    type i,
        lv_temp_2  type i,
        lv_second  type i,
        lv_changed type c,
        lv_perm    type i.
  describe table iv_set lines lv_len.

  lv_perm = lv_len - 1.
  lv_changed = ' '.
  " Loop backwards through the table, attempting to find elements which
  " can be permuted. If we find one, break out of the table and set the
  " flag indicating a switch.
  do.
    if lv_perm <= 0.
      exit.
    endif.
    " Read the elements.
    read table iv_set index lv_perm into lv_first.
    add 1 to lv_perm.
    read table iv_set index lv_perm into lv_second.
    subtract 1 from lv_perm.
    if lv_first < lv_second.
      lv_changed = 'X'.
      exit.
    endif.
    subtract 1 from lv_perm.
  enddo.

  " Last permutation.
  if lv_changed <> 'X'.
    ev_last = 'X'.
    exit.
  endif.

  " Swap tail decresing to get a tail increasing.
  lv_count = lv_perm + 1.
  do.
    lv_first = lv_len + lv_perm - lv_count + 1.
    if lv_count >= lv_first.
      exit.
    endif.

    read table iv_set index lv_count into lv_temp.
    read table iv_set index lv_first into lv_temp_2.
    modify iv_set index lv_count from lv_temp_2.
    modify iv_set index lv_first from lv_temp.
    add 1 to lv_count.
  enddo.

  lv_count = lv_len - 1.
  do.
    if lv_count <= lv_perm.
      exit.
    endif.

    read table iv_set index lv_count into lv_first.
    read table iv_set index lv_perm into lv_second.
    read table iv_set index lv_len into lv_third.
    if ( lv_first < lv_third ) and ( lv_first > lv_second ).
      lv_len = lv_count.
    endif.

    subtract 1 from lv_count.
  enddo.

  read table iv_set index lv_perm into lv_temp.
  read table iv_set index lv_len into lv_temp_2.
  modify iv_set index lv_perm from lv_temp_2.
  modify iv_set index lv_len from lv_temp.
endform.
