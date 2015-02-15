       identification division.
       program-id. dowtest.
       data division.
       working-storage section.
       01  ws-inp-date   pic x(08).
       01  filler redefines ws-inp-date.
         03  ws-inp-year  pic 9(04).
       01  ws-dow        pic 9(05).
       procedure division.
           move '00001225' to ws-inp-date
           perform test before
           varying ws-inp-year from 2008 by +1
           until ws-inp-year > 2121
             call "todow" using
                 by reference ws-inp-date
                 by reference ws-dow
                 if ws-dow = 1 then
                   display 'year=' ws-inp-year
                 end-if
           end-perform
           stop run.

       end program dowtest.

       identification division.
       program-id.  todow.
       environment division.
       input-output section.
       file-control.
       data division.
       file section.
       working-storage section.
       01 tally pic 9(05).
       01  wms-work-area.
         03  wms-year       pic 9(04).
         03  wms-month      pic 9(02).
         03  wms-csys       pic 9(01) value 1.
         03  wms-sum        pic 9(05).
       linkage section.
       01  lkip-date.
         03  lkip-date-year     pic 9(04).
         03  lkip-date-month    pic 9(02).
         03  lkip-date-day      pic 9(02).
       01  lkop-dow             pic 9(05).
         88  lkop-sunday                   value 1.
       procedure division using
           by reference lkip-date
           by reference lkop-dow
           .

           if lkip-date-month < 3
             compute wms-month = lkip-date-month + 12
             compute wms-year  = lkip-date-year - 1
           else
             compute wms-month = lkip-date-month
             compute wms-year  = lkip-date-year
           end-if

          compute wms-sum    =
                          ( lkip-date-day + 2 * wms-month + wms-year
                          + function integer (6 * (wms-month + 1) / 10)
                          + function integer ( wms-year / 4   )
                          - function integer ( wms-year / 100 )
                          + function integer ( wms-year / 400 )
                          + wms-csys )
         compute lkop-dow = function mod (wms-sum, 7) + 1
                          .
       end program todow.
