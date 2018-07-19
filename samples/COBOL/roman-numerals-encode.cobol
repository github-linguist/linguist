IDENTIFICATION DIVISION.
PROGRAM-ID. TOROMAN.
DATA DIVISION.
working-storage section.
  01 ws-number pic 9(4) value 0.
  01 ws-save-number pic 9(4).
  01 ws-tbl-def.
    03 filler pic x(7) value '1000M  '.
    03 filler pic x(7) value '0900CM '.
    03 filler pic x(7) value '0500D  '.
    03 filler pic x(7) value '0400CD '.
    03 filler pic x(7) value '0100C  '.
    03 filler pic x(7) value '0090XC '.
    03 filler pic x(7) value '0050L  '.
    03 filler pic x(7) value '0040XL '.
    03 filler pic x(7) value '0010X  '.
    03 filler pic x(7) value '0009IX '.
    03 filler pic x(7) value '0005V  '.
    03 filler pic x(7) value '0004IV '.
    03 filler pic x(7) value '0001I  '.
  01  filler redefines ws-tbl-def.
    03 filler occurs 13 times indexed by rx.
      05 ws-tbl-divisor    pic 9(4).
      05 ws-tbl-roman-ch   pic x(1) occurs 3 times indexed by cx.
  01 ocx pic 99.
  01 ws-roman.
    03 ws-roman-ch         pic x(1) occurs 16 times.
PROCEDURE DIVISION.
  accept ws-number
  perform
  until ws-number = 0
    move ws-number to ws-save-number
    if ws-number > 0 and ws-number < 4000
      initialize ws-roman
      move 0 to ocx
      perform varying rx from 1 by +1
      until ws-number = 0
        perform until ws-number < ws-tbl-divisor (rx)
          perform varying cx from 1 by +1
  		  until ws-tbl-roman-ch (rx, cx) = spaces
            compute ocx = ocx + 1
            move ws-tbl-roman-ch (rx, cx) to ws-roman-ch (ocx)
          end-perform
          compute ws-number = ws-number - ws-tbl-divisor (rx)
        end-perform
      end-perform
      display 'inp=' ws-save-number ' roman=' ws-roman
    else
      display 'inp=' ws-save-number ' invalid'
    end-if
    accept ws-number
  end-perform
  .
