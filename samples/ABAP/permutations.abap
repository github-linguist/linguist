data: lv_flag type c,
      lv_number type i,
      lt_numbers type table of i.

append 1 to lt_numbers.
append 2 to lt_numbers.
append 3 to lt_numbers.

do.
  perform permute using lt_numbers changing lv_flag.
  if lv_flag = 'X'.
    exit.
  endif.
  loop at lt_numbers into lv_number.
    write (1) lv_number no-gap left-justified.
    if sy-tabix <> '3'.
      write ', '.
    endif.
  endloop.
  skip.
enddo.

" Permutation function - this is used to permute:
" Can be used for an unbounded size set.
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
