METHOD luhn_check.

  DATA: sum(1) TYPE n VALUE 0. " Sum of checksum.
  DATA: current TYPE i. " Current digit.
  DATA: odd TYPE i VALUE 1. " Multiplier.
  DATA: len TYPE i. " String crowler.


  " Luhn algorithm.
  len = NUMOFCHAR( pi_string ) - 1.
  WHILE ( len >= 0 ).
    current = pi_string+len(1) * odd.
    IF ( current > 9 ).
      current = current - 9. " Digits sum.
    ENDIF.
    sum = sum + current.
    odd = 3 - odd. " 1 <--> 2 Swich
    len = len - 1. " Move to next charcter.
  ENDWHILE.

  " Validation check.
  IF ( sum = 0 ).
    pr_valid = abap_true.
  ELSE.
    pr_valid = abap_false.
  ENDIF.

ENDMETHOD.
