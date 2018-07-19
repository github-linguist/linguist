REPORT z99bottles.

DATA lv_no_bottles(2) TYPE n VALUE 99.

DO lv_no_bottles TIMES.
  WRITE lv_no_bottles NO-ZERO.
  WRITE ' bottles of beer on the wall'.
  NEW-LINE.
  WRITE lv_no_bottles NO-ZERO.
  WRITE ' bottles of beer'.
  NEW-LINE.
  WRITE 'Take one down, pass it around'.
  NEW-LINE.
  SUBTRACT 1 FROM lv_no_bottles.
  WRITE lv_no_bottles NO-ZERO.
  WRITE ' bottles of beer on the wall'.
  WRITE /.
ENDDO.
