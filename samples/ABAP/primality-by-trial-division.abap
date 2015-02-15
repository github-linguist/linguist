class ZMLA_ROSETTA definition
  public
  create public .

public section.

  types:
    enumber         TYPE          N  LENGTH 60 .
  types:
    listof_enumber  TYPE TABLE OF enumber .

  class-methods IS_PRIME
    importing
      value(N) type ENUMBER
    returning
      value(OFLAG) type ABAP_BOOL .
  class-methods IS_PRIME_I
    importing
      value(N) type I
    returning
      value(OFLAG) type ABAP_BOOL .
  protected section.
  private section.
ENDCLASS.



CLASS ZMLA_ROSETTA IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZMLA_ROSETTA=>IS_PRIME
* +-------------------------------------------------------------------------------------------------+
* | [--->] N                              TYPE        ENUMBER
* | [<-()] OFLAG                          TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method IS_PRIME.
    IF n < 2.
      oflag = abap_false.
      RETURN.
    ENDIF.
    IF n = 2 or n = 3.
      oflag = abap_true.
      RETURN.
    ENDIF.
    IF n mod 2 = 0 or n mod 3 = 0.
      oflag = abap_false.
      RETURN.
    ENDIF.
    DATA: lim type enumber,
          d   type enumber,
          i   TYPE i        VALUE 2.
    lim = sqrt( n ).
    d   = 5.
    WHILE d <= lim.
      IF n mod d = 0.
        oflag = abap_false.
        RETURN.
      ENDIF.
      d = d + i.
      i = 6 - i.  " this modifies 2 into 4 and viceversa
    ENDWHILE.
    oflag = abap_true.
    RETURN.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZMLA_ROSETTA=>IS_PRIME_I
* +-------------------------------------------------------------------------------------------------+
* | [--->] N                              TYPE        I
* | [<-()] OFLAG                          TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method IS_PRIME_I.
    IF n < 2.
      oflag = abap_false.
      RETURN.
    ENDIF.
    IF n = 2 or n = 3.
      oflag = abap_true.
      RETURN.
    ENDIF.
    IF n mod 2 = 0 or n mod 3 = 0.
      oflag = abap_false.
      RETURN.
    ENDIF.
    DATA: lim type i,
          d   type i,
          i   TYPE i        VALUE 2.
    lim = sqrt( n ).
    d   = 5.
    WHILE d <= lim.
      IF n mod d = 0.
        oflag = abap_false.
        RETURN.
      ENDIF.
      d = d + i.
      i = 6 - i.  " this modifies 2 into 4 and viceversa
    ENDWHILE.
    oflag = abap_true.
    RETURN.
  endmethod.
ENDCLASS.
