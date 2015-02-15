report z_sum_a_b.
data: lv_output type i.
selection-screen begin of block input.
  parameters:
    p_first type i,
    p_second type i.
selection-screen end of block input.

at selection-screen output.
  %_p_first_%_app_%-text  = 'First Number: '.
  %_p_second_%_app_%-text = 'Second Number: '.

start-of-selection.
  lv_output = p_first + p_second.
  write : / lv_output.
