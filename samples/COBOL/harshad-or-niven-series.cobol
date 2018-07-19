identification division.
program-id. harshad.
environment division.
data division.
working-storage section.
*> for storing first 20 harshad-niven numbers
01  harshads.
    03  harshad pic 9(5)    occurs 20 times indexed by niven.

*> numbers tested for harshad-niven-ness.
01  first-num   pic 9(5).
01  second-num  pic 9(5).

*> loop counter
01  i   pic 9(5).

*> for calculating sum of digits
01  div pic 9(5).
01  mod pic 9(5).
01  tot pic 9(5).

*> for harshad-niven calculation and display
01  harshad-div pic 9(5).
01  harshad-mod pic 9(5).
    88  evenly-divisible    value 0.
01  harshad-disp    pic zzzz9.
01  harshad-result  pic 9(5).

*> for selecting what to do with results of harshad calculation
01  pass        pic 9.
    88  first-pass  value 1.
    88  second-pass value 2.

procedure division.
10-main section.
    move 1 to pass.
    set niven to 1.
    perform 20-calculate-harshad with test before varying first-num from 1 by 1 until niven = 21.

    move 2 to pass.
    move first-num to second-num.
    perform 20-calculate-harshad with test after varying first-num from second-num by 1 until harshad-result > 1000.

    perform with test after varying i from 1 by 1 until i = 20
        move harshad(i) to harshad-disp
        display function trim(harshad-disp) space with no advancing
    end-perform.

    move harshad-result to harshad-disp.
    display "... " function trim(harshad-disp).
    stop run.

20-calculate-harshad.
    move first-num to div.
    move zero to harshad-result.
    perform 100-calculate-sum-of-digits.
    divide first-num by tot giving harshad-div remainder harshad-mod.
    if evenly-divisible
        if first-pass
            move first-num to harshad(niven)
            set niven up by 1
        else
            move first-num to harshad-result
        end-if
    end-if.
    exit paragraph.

100-calculate-sum-of-digits.
    move zero to tot.
    perform with test after until div = 0
        divide div by 10 giving div remainder mod
        add mod to tot
    end-perform.
    *> if tot >= 10
    *>  move tot to div
    *>  go to 100-calculate-sum-of-digits
    *> end-if.
    exit paragraph.
