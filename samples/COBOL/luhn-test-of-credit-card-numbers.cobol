       IDENTIFICATION DIVISION.
       PROGRAM-ID.  LUHNTEST.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       data division.
       WORKING-STORAGE SECTION.
       01  inp-card.
         03  inp-card-ch      pic x(01) occurs 20 times.
       01  ws-result          pic 9(01).
         88  pass-luhn-test             value 0.

       PROCEDURE DIVISION.
           move "49927398716"       to inp-card
           perform test-card
           move "49927398717"       to inp-card
           perform test-card
           move "1234567812345678"  to inp-card
           perform test-card
           move "1234567812345670"  to inp-card
           perform test-card
           stop run
           .
       test-card.
           call "LUHN" using inp-card, ws-result
           if pass-luhn-test
             display "input=" inp-card "pass"
           else
             display "input=" inp-card "fail"
           .

       END PROGRAM LUHNTEST.
       IDENTIFICATION DIVISION.
       PROGRAM-ID.  LUHN.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  maxlen           pic 9(02) comp value 16.
       01  inplen           pic 9(02) comp value 0.
       01  i                pic 9(02) comp value 0.
       01  j                pic 9(02) comp value 0.
       01  l                pic 9(02) comp value 0.
       01  dw               pic 9(02) comp value 0.
       01  ws-total         pic 9(03) comp value 0.
       01  ws-prod          pic 99.
       01  filler redefines ws-prod.
         03  ws-prod-tens   pic 9.
         03  ws-prod-units  pic 9.
       01  ws-card.
         03  filler           occurs 16 times depending on maxlen.
           05  ws-card-ch     pic x(01).
           05  ws-card-digit redefines ws-card-ch  pic 9(01).
       LINKAGE SECTION.
       01  inp-card.
         03  inp-card-ch      pic x(01) occurs 20 times.
       01  ws-result          pic 9(01).
         88  pass-luhn-test             value 0.

       PROCEDURE DIVISION using inp-card, ws-result.
           perform varying i from 1 by +1
           until i > maxlen
           or    inp-card-ch (i) = space
           end-perform
           compute l = i - 1
           compute inplen = l
           perform varying j from 1 by +1
           until j > inplen
             if l < 1
               move "0"             to ws-card-ch (j)
             else
               move inp-card-ch (l) to ws-card-ch (j)
               compute l = l - 1
             end-if
           end-perform
           move 0 to ws-total
           perform varying i from 1 by +1
           until i > inplen
             compute dw = 2 - (i - 2 * function integer (i / 2))
             compute ws-prod = ws-card-digit (i) * dw
             compute ws-total = ws-total
                              + ws-prod-tens
                              + ws-prod-units
           end-perform
           compute ws-result = ws-total - 10 * function integer (ws-total / 10)
           goback
           .
       END PROGRAM LUHN.
