REPORT  zhuberv_ackermann.

CLASS zcl_ackermann DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS ackermann IMPORTING m TYPE i
                                      n TYPE i
                            RETURNING value(v) TYPE i.
ENDCLASS.            "zcl_ackermann DEFINITION


CLASS zcl_ackermann IMPLEMENTATION.

  METHOD: ackermann.

    DATA: lv_new_m TYPE i,
          lv_new_n TYPE i.

    IF m = 0.
      v = n + 1.
    ELSEIF m > 0 AND n = 0.
      lv_new_m = m - 1.
      lv_new_n = 1.
      v = ackermann( m = lv_new_m n = lv_new_n ).
    ELSEIF m > 0 AND n > 0.
      lv_new_m = m - 1.

      lv_new_n = n - 1.
      lv_new_n = ackermann( m = m n = lv_new_n ).

      v = ackermann( m = lv_new_m n = lv_new_n ).
    ENDIF.

  ENDMETHOD.                    "ackermann

ENDCLASS.                    "zcl_ackermann IMPLEMENTATION


PARAMETERS: pa_m TYPE i,
            pa_n TYPE i.

DATA: lv_result TYPE i.

START-OF-SELECTION.
  lv_result = zcl_ackermann=>ackermann( m = pa_m n = pa_n ).
  WRITE: / lv_result.
