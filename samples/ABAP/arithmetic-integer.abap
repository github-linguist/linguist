report zz_arithmetic no standard page heading.

" Read in the two numbers from the user.
selection-screen begin of block input.
  parameters: p_first type i,
              p_second type i.
selection-screen end of block input.

" Set the text value that is displayed on input request.
at selection-screen output.
  %_p_first_%_app_%-text  = 'First Number: '.
  %_p_second_%_app_%-text = 'Second Number: '.

end-of-selection.
  data: lv_result type i.
  lv_result = p_first + p_second.
  write: / 'Addition:', lv_result.
  lv_result = p_first - p_second.
  write: / 'Substraction:', lv_result.
  lv_result = p_first * p_second.
  write: / 'Multiplication:', lv_result.
  lv_result = p_first div p_second.
  write: / 'Integer quotient:', lv_result. " Truncated towards zero.
  lv_result = p_first mod p_second.
  write: / 'Remainder:',  lv_result.
