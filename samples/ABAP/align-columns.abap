report z_align no standard page header.
start-of-selection.

data: lt_strings type standard table of string,
      lv_strings type string.
append: 'Given$a$text$file$of$many$lines,$where$fields$within$a$line$' to lt_strings,
        'are$delineated$by$a$single$''dollar''$character,$write$a$program' to lt_strings,
        'that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$' to lt_strings,
        'column$are$separated$by$at$least$one$space.' to lt_strings,
        'Further,$allow$for$each$word$in$a$column$to$be$either$left$' to lt_strings,
        'justified,$right$justified,$or$center$justified$within$its$column.' to lt_strings.
types ty_strings type standard table of string.

perform align_col using 'LEFT' lt_strings.
skip.
perform align_col using 'RIGHT' lt_strings.
skip.
perform align_col using 'CENTER' lt_strings.


form align_col using iv_just type string iv_strings type ty_strings.
  constants: c_del value '$'.
  data: lv_string type string,
        lt_strings type table of string,
        lt_tables like table of lt_strings,
        lv_first type string,
        lv_second type string,
        lv_longest type i value 0,
        lv_off type i value 0,
        lv_len type i.
  " Loop through the supplied text. It is expected at the input is a table of strings, with each
  " entry in the table representing a new line of the input.
  loop at iv_strings into lv_string.
    " Split the current line at the delimiter.
    split lv_string at c_del into lv_first lv_second.
    " Loop through the line splitting at every delimiter.
    do.
      append lv_first to lt_strings.
      lv_len = strlen( lv_first ).
      " Check if the length of the new string is greater than the currently stored length.
      if lv_len > lv_longest.
        lv_longest = lv_len.
      endif.
      if lv_second na c_del.
        " Check if the string is longer than the recorded maximum.
        lv_len = strlen( lv_second ).
        if lv_len > lv_longest.
          lv_longest = lv_len.
        endif.
        append lv_second to lt_strings.
        exit.
      endif.
      split lv_second at c_del into lv_first lv_second.
    enddo.

    append lt_strings to lt_tables.
    clear lt_strings.
  endloop.

  " Loop through each line of input.
  loop at lt_tables into lt_strings.
    " Loop through each word in the line (Separated by specified delimiter).
    loop at lt_strings into lv_string.
      lv_off = ( sy-tabix - 1 ) * ( lv_longest + 2 ).
      case iv_just.
        when 'LEFT'.
          write : at (lv_longest) lv_string left-justified.
        when 'RIGHT'.
          write at (lv_longest) lv_string right-justified.
        when 'CENTER'.
          write at (lv_longest) lv_string centered.
      endcase.
    endloop.
    skip.
    sy-linno = sy-linno - 1.
  endloop.
endform.
