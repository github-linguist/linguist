      **** sndxtest *********************************************
      * Demonstrate the soundex encoding functions.
      ***************************************************************
       Identification division.
       Program-id. sndxtest.

       Data division.
       Working-storage section.
       01 sample-word-list.
           05 sample-words.
               10 filler pic x(15) value "soundex".
               10 filler pic x(15) value "example".
               10 filler pic x(15) value "sownteks".
               10 filler pic x(15) value "ekzampul".
               10 filler pic x(15) value "Euler".
               10 filler pic x(15) value "Gauss".
               10 filler pic x(15) value "Hilbert".
               10 filler pic x(15) value "Knuth".
               10 filler pic x(15) value "Lloyd".
               10 filler pic x(15) value "Lukasiewicz".
               10 filler pic x(15) value "Ellery".
               10 filler pic x(15) value "ghosh".
               10 filler pic x(15) value "Heilbronn".
               10 filler pic x(15) value "Kand".
               10 filler pic x(15) value "Ladd".
               10 filler pic x(15) value "lissajous".
               10 filler pic x(15) value "Wheaton".
               10 filler pic x(15) value "Burroughs".
               10 filler pic x(15) value "burrows".
               10 filler pic x(15) value "O'Hara".
               10 filler pic x(15) value "Washington".
               10 filler pic x(15) value "lee".
               10 filler pic x(15) value "Gutierrez".
               10 filler pic x(15) value "Phister".
               10 filler pic x(15) value "Jackson".
               10 filler pic x(15) value "tymczak".
               10 filler pic x(15) value "Vandeusen".
               10 filler pic x(15) value "Ashcraft".
           05 sample-word redefines sample-words
                         pic x(15) occurs 28 times indexed by wrd-idx.
       01 wrd-code       pic x999.

       Procedure division.
           Perform varying wrd-idx from 1 by 1
           until wrd-idx greater than 28
               call "sndxenc" using
                   by reference sample-word(wrd-idx)
                   by reference wrd-code
               display wrd-code " " sample-word(wrd-idx)
           end-perform.
           Stop run.

       End program sndxtest.

      *** sndxenc ********************************************
      * Given a string return its soundex encoding.
      ***************************************************************
       Identification division.
       Program-id. sndxenc.

       Data division.
       Local-storage section.
       01 str-idx            pic 99.
       01 let-code           pic  9.
       01 prv-let-code       pic  9.
       01 sdx-idx            pic  9  value 1.

       Linkage section.
       01 str-to-encode.
           05 str-first-let  pic x.
           05 str-rest-let   pic x  occurs 14 times.
       01 sdx-code.
           05 sdx-first-let  pic x.
           05 sdx-nums       pic 9  occurs  3 times.

       Procedure division using
           by reference str-to-encode
           by reference sdx-code.
           Perform encode-start thru encode-done.
           Goback.

       Encode-start.
           Move zeros to sdx-code.
           Move function upper-case(str-first-let) to sdx-first-let.
           Call "sndxchar" using
               by reference str-first-let
               by reference let-code.
           Move let-code to prv-let-code.

       Encode-string.
           Perform varying str-idx from 1 by 1
               until str-idx greater than 15
               or str-rest-let(str-idx) = space
               or sdx-idx greater than 3
               call "sndxchar" using
                   by reference str-rest-let(str-idx)
                   by reference let-code
               if let-code not equal 7 then
                   if let-code not equal 0
                   and let-code not equal prv-let-code
                       move let-code to sdx-nums(sdx-idx)
                       add 1 to sdx-idx
                   end-if
                   move let-code to prv-let-code
               end-if
           end-perform.

       Encode-done.
           continue.
       End program sndxenc.


      *** sndxchar **********************************************
      * Given a character, return its soundex encoding.
      * Code 7 is for h or w, which an encoder should ignore when
      * either one separates double letters.
      ***************************************************************
       Identification division.
       Program-id. sndxchar.

       Data division.
       Local-storage section.
       01 lc-chr pic x.
           88 code1 value "b", "f", "p", "v".
           88 code2 value "c", "g", "j", "k", "q", "s", "x", "z".
           88 code3 value "d", "t".
           88 code4 value "l".
           88 code5 value "m", "n".
           88 code6 value "r".
           88 code7 value "h", "w".

       Linkage section.
       01 char-to-encode pic x.
       01 char-sdx-code  pic 9.

       Procedure division using
           by reference char-to-encode
           by reference char-sdx-code.
           Move function lower-case(char-to-encode) to lc-chr.
           If          code1 then move 1 to char-sdx-code
               else if code2 then move 2 to char-sdx-code
               else if code3 then move 3 to char-sdx-code
               else if code4 then move 4 to char-sdx-code
               else if code5 then move 5 to char-sdx-code
               else if code6 then move 6 to char-sdx-code
               else if code7 then move 7 to char-sdx-code
               else               move 0 to char-sdx-code
           end-if.
       End program sndxchar.
