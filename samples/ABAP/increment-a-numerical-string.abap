report zz_incstring
perform test using: '0', '1', '-1', '10000000', '-10000000'.

form test using iv_string type string.
  data: lv_int  type i,
        lv_string type string.
  lv_int = iv_string + 1.
  lv_string = lv_int.
  concatenate '"' iv_string '" + 1 = "' lv_string '"' into lv_string.
  write / lv_string.
endform.
