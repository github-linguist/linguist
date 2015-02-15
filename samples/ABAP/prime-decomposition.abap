class ZMLA_ROSETTA definition
  public
  create public .

  public section.

    types:
      enumber         TYPE          N  LENGTH 60,
      listof_enumber  TYPE TABLE OF enumber .

    class-methods FACTORS
      importing
        value(N) type ENUMBER
      exporting
        value(ORET) type LISTOF_ENUMBER .
  protected section.
  private section.
ENDCLASS.



CLASS ZMLA_ROSETTA IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZMLA_ROSETTA=>FACTORS
* +-------------------------------------------------------------------------------------------------+
* | [--->] N                              TYPE        ENUMBER
* | [<---] ORET                           TYPE        LISTOF_ENUMBER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method FACTORS.
    CLEAR oret.
    WHILE n mod 2 = 0.
      n = n / 2.
      APPEND 2 to oret.
    ENDWHILE.
    DATA: lim type enumber,
          i   type enumber.
    lim = sqrt( n ).
    i   = 3.
    WHILE i <= lim.
      WHILE n mod i = 0.
        APPEND i to oret.
        n = n / i.
        lim = sqrt( n ).
      ENDWHILE.
      i = i + 2.
    ENDWHILE.
    IF n > 1.
      APPEND n to oret.
    ENDIF.
  endmethod.
ENDCLASS.
