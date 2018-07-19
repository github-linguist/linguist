      IDENTIFICATION DIVISION.
      PROGRAM-ID.  UNROMAN.
      DATA DIVISION.
      WORKING-STORAGE SECTION.
      01  filler.
        03  i              pic 9(02) comp.
        03  j              pic 9(02) comp.
        03  k              pic 9(02) comp.
        03  l              pic 9(02) comp.
      01  inp-roman.
        03  inp-rom-ch     pic x(01) occurs 20 times.
      01  inp-roman-digits.
        03  inp-rom-digit  pic 9(01) occurs 20 times.
      01  ws-search-idx        pic 9(02) comp.
      01  ws-tbl-table-def.
        03  filler pic x(05) value '1000M'.
        03  filler pic x(05) value '0500D'.
        03  filler pic x(05) value '0100C'.
        03  filler pic x(05) value '0050L'.
        03  filler pic x(05) value '0010X'.
        03  filler pic x(05) value '0005V'.
        03  filler pic x(05) value '0001I'.
      01  filler redefines ws-tbl-table-def.
        03  ws-tbl-roman      occurs 07 times indexed by rx.
          05  ws-tbl-rom-val  pic 9(04).
          05  ws-tbl-rom-ch   pic x(01).
      01  ws-number           pic s9(05) value 0.
      01  ws-number-pic       pic zzzz9-.

      PROCEDURE DIVISION.
          accept inp-roman
          perform
          until inp-roman = ' '
            move zeroes to inp-roman-digits
            perform
            varying i from 1 by +1 until inp-rom-ch (i) = ' '
              set rx to 1
              search ws-tbl-roman
                at end
                  move 0 to inp-rom-digit (i)
                when ws-tbl-rom-ch (rx) = inp-rom-ch (i)
                  set inp-rom-digit (i) to rx
              end-search
            end-perform
            compute l = i - 1
            move 0 to ws-number
            perform
            varying i from 1 by +1
            until i > l or inp-rom-digit (i) = 0
              compute j = inp-rom-digit (i)
              compute k = inp-rom-digit (i + 1)
              if ws-tbl-rom-val (k)
              >  ws-tbl-rom-val (j)
                compute ws-number
                =      ws-number
                -      ws-tbl-rom-val (j)
              else
                compute ws-number
                =      ws-number
                +      ws-tbl-rom-val (j)
              end-if
            end-perform
            move ws-number to ws-number-pic
            display '----------'
            display 'roman=' inp-roman
            display 'arabic=' ws-number-pic
            if i < l or ws-number = 0
              display 'invalid/incomplete roman numeral at pos 'i
                      ' found ' inp-rom-ch (i)
            end-if
            accept inp-roman
          end-perform
          stop run
          .
      END PROGRAM UNROMAN.
