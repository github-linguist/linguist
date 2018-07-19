report zz_anagrams no standard page heading.
define update_progress.
  call function 'SAPGUI_PROGRESS_INDICATOR'
    exporting
      text = &1.
end-of-definition.

" Selection screen segment allowing the person to choose which file will act as input.
selection-screen begin of block file_choice.
  parameters p_file type string lower case.
selection-screen end of block file_choice.

" When the user requests help with input, run the routine to allow them to navigate the presentation server.
at selection-screen on value-request for p_file.
  perform getfile using p_file.

at selection-screen output.
  %_p_file_%_app_%-text = 'Input File: '.

start-of-selection.
  data: gt_data type table of string.

  " Read the specified file from the presentation server into memory.
  perform readfile using p_file changing gt_data.
  " After the file has been read into memory, loop through it line-by-line and make anagrams.
  perform anagrams using gt_data.

" Subroutine for generating a list of anagrams.
" The supplied input is a table, with each entry corresponding to a word.
form anagrams using it_data like gt_data.
  types begin of ty_map.
    types key type string.
    types value type string.
  types end of ty_map.

  data: lv_char     type c,
        lv_len      type i,
        lv_string   type string,
        ls_entry    type ty_map,
        lt_anagrams type standard table of ty_map,
        lt_c_tab    type table of string.

  field-symbols: <fs_raw> type string.
  " Loop through each word in the table, and make an associative array.
  loop at gt_data assigning <fs_raw>.
    " First, we need to re-order the word alphabetically. This generated a key. All anagrams will use this same key.
    " Add each character to a table, which we will then sort alphabetically.
    lv_len = strlen( <fs_raw> ).
    refresh lt_c_tab.
    do lv_len times.
      lv_len = sy-index  - 1.
      append <fs_raw>+lv_len(1) to lt_c_tab.
    enddo.
    sort lt_c_tab as text.
    " Now append the characters to a string and add it as a key into the map.
    clear lv_string.
    loop at lt_c_tab into lv_char.
      concatenate lv_char lv_string into lv_string respecting blanks.
    endloop.
    ls_entry-key = lv_string.
    ls_entry-value = <fs_raw>.
    append ls_entry to lt_anagrams.
  endloop.
  " After we're done processing, output a list of the anagrams.
  clear lv_string.
  loop at lt_anagrams into ls_entry.
    " Is it part of the same key --> Output in the same line, else a new entry.
    if lv_string = ls_entry-key.
        write: ', ', ls_entry-value.
    else.
      if sy-tabix <> 1.
        write: ']'.
      endif.
      write:  / '[', ls_entry-value.
    endif.
    lv_string = ls_entry-key.
  endloop.
  " Close last entry.
  write ']'.
endform.

" Read a specified file from the presentation server.
form readfile using i_file type string changing it_raw like gt_data.
  data: l_datat type string,
        l_msg(2048),
        l_lines(10).

  " Read the file into memory.
  update_progress 'Reading file...'.
  call method cl_gui_frontend_services=>gui_upload
    exporting
      filename = i_file
    changing
      data_tab = it_raw
    exceptions
      others   = 1.
  " Output error if the file could not be uploaded.
  if sy-subrc <> 0.
    write : / 'Error reading the supplied file!'.
    return.
  endif.
endform.
