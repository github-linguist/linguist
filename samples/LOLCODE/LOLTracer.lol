HAI 1.3
    OBTW
      Author: Logan Kelly (logan.kelly@gmail.com)
      Github: https://github.com/LoganKelly/LOLTracer
    TLDR

    OBTW prev is the number used in the randin function.
         I had to declare it in global scope so that it
         would retain its value between calls to randin.
    TLDR
    I HAS A prev ITZ 0
    I HAS A rand_max ITZ 104729


    OBTW Equivalent to C's rand() function, except returns
        a number in the range of 0 to rand_max.
    TLDR
    HOW IZ I randin
        I HAS A a ITZ 33083
        I HAS A c ITZ 67607
        prev R MOD OF SUM OF PRODUKT OF prev AN a AN c AN rand_max
        FOUND YR prev
    IF U SAY SO


    BTW Returns a random number within the range of 0-1.
    HOW IZ I rand_onein
        I HAS A rand_num ITZ I IZ randin MKAY
        rand_num IS NOW A NUMBAR
        I HAS A rand_max_float ITZ MAEK rand_max A NUMBAR
        FOUND YR  QUOSHUNT OF rand_num AN rand_max_float
    IF U SAY SO


    OBTW Equivalent to C ceil() function. Returns the next
        largest integer for the given number.
    TLDR
    HOW IZ I ceilin YR num
        I HAS A int_num ITZ num
        int_num IS NOW A NUMBR
        BOTH SAEM int_num AN num, O RLY?
            YA RLY, FOUND YR num
        OIC
        DIFFRINT num AN SMALLR OF num AN 0, O RLY?
            YA RLY
                int_num R SUM OF int_num AN 1
                FOUND YR MAEK int_num A NUMBAR
        OIC
        DIFFRINT num AN BIGGR OF num AN 0, O RLY?
            YA RLY
                FOUND YR MAEK int_num A NUMBAR
        OIC
    IF U SAY SO


    OBTW Convert a number to hexadecimal. This
         is returned as a string.
    TLDR
    HOW IZ I decimal_to_hex YR num
        I HAS A i ITZ 0
        I HAS A rem
        I HAS A hex_num ITZ A BUKKIT
        I HAS A decimal_num ITZ num
        IM IN YR num_loop
            rem R MOD OF decimal_num AN 16
            I HAS A hex_digit
            rem, WTF?
                OMG 10, hex_digit R "A", GTFO
                OMG 11, hex_digit R "B", GTFO
                OMG 12, hex_digit R "C", GTFO
                OMG 13, hex_digit R "D", GTFO
                OMG 14, hex_digit R "E", GTFO
                OMG 15, hex_digit R "F", GTFO
                OMGWTF, hex_digit R rem
            OIC
            hex_num HAS A SRS i ITZ hex_digit
            decimal_num R QUOSHUNT OF decimal_num AN 16
            BOTH SAEM decimal_num AN 0, O RLY?
                YA RLY, GTFO
                NO WAI, i R SUM OF i AN 1
            OIC
        IM OUTTA YR num_loop
        I HAS A hex_string ITZ A YARN
        IM IN YR string_reverse
            DIFFRINT i AN BIGGR OF i AN 0, O RLY?
                YA RLY, GTFO
            OIC
            hex_string R SMOOSH hex_string AN hex_num'Z SRS i MKAY
            i R DIFF OF i AN 1
        IM OUTTA YR string_reverse
        FOUND YR hex_string
    IF U SAY SO


    OBTW Convert a number to binary. This is returned
         as a bukkit which has slots number 0-N where
         n is equal to the number of binary digits - 1.
         It also has a length slot which is equal to
         the number of binary digits.
    TLDR
    HOW IZ I decimal_to_binary YR num
        I HAS A i ITZ 0
        I HAS A decimal_num ITZ num
        I HAS A binary_num ITZ A BUKKIT
        IM IN YR num_loop
            binary_num HAS A SRS i ITZ MOD OF decimal_num AN 2
            decimal_num R QUOSHUNT OF decimal_num AN 2
            BOTH SAEM decimal_num AN 0, O RLY?
                YA RLY
                    I HAS A length ITZ SUM OF i AN 1
                    binary_num HAS A length ITZ length
                    GTFO
                NO WAI, i R SUM OF i AN 1
            OIC
        IM OUTTA YR num_loop
        FOUND YR binary_num
    IF U SAY SO


    OBTW Bitwise and two binary numbers. The numbers
         must be provided in the format returned by
         decimal_to_binary.
    TLDR
    HOW IZ I bitwise_andin YR first_num AN YR second_num
        I HAS A binary_first_num ITZ I IZ decimal_to_binary YR first_num MKAY
        I HAS A binary_second_num ITZ I IZ decimal_to_binary YR second_num MKAY
        I HAS A first_length ITZ binary_first_num'Z length
        I HAS A second_length ITZ binary_second_num'Z length
        I HAS A max_length ITZ BIGGR OF first_length AN second_length
        I HAS A final_binary ITZ A BUKKIT
        I HAS A final_length ITZ 0
        I HAS A i ITZ 0
        IM IN YR num_loop
            BOTH SAEM i AN max_length, O RLY?
                YA RLY, GTFO
            OIC
            I HAS A first_binary ITZ 0
            I HAS A second_binary ITZ 0
            DIFFRINT i AN BIGGR OF i AN first_length, O RLY?
                YA RLY, first_binary R binary_first_num'Z SRS i
            OIC
            DIFFRINT i AN BIGGR OF i AN second_length, O RLY?
                YA RLY, second_binary R binary_second_num'Z SRS i
            OIC
            EITHER OF BOTH SAEM first_binary AN 0 AN ...
                      BOTH SAEM second_binary AN 0, O RLY?
                YA RLY, final_binary HAS A SRS i ITZ 0
                NO WAI
                    final_binary HAS A SRS i ITZ 1
                    final_length R SUM OF i AN 1
            OIC
            i R SUM OF i AN 1
        IM OUTTA YR num_loop
        final_binary HAS A length ITZ final_length
        FOUND YR final_binary
    IF U SAY SO


    OBTW Bitshift left a binary number by num_bits.
         The binary number must be provided in the format
         returned by decimal_to_binary.
    TLDR
    HOW IZ I bit_shift_leftin YR num AN YR num_bits
        I HAS A binary_num ITZ num
        I HAS A length ITZ binary_num'Z length
        I HAS A i ITZ SUM OF DIFF OF length AN 1 AN num_bits
        I HAS A shifted_binary_num ITZ A BUKKIT
        IM IN YR num_loop
            BOTH SAEM i AN -1, O RLY?
                YA RLY, GTFO
            OIC
            I HAS A unshifted_index ITZ DIFF OF i AN num_bits
            BOTH SAEM unshifted_index AN BIGGR OF unshifted_index AN 0, O RLY?
                YA RLY
                    shifted_binary_num HAS A SRS i ITZ binary_num'Z SRS unshifted_index
                NO WAI
                    shifted_binary_num HAS A SRS i ITZ 0
            OIC
            i R DIFF OF i AN 1
        IM OUTTA YR num_loop
        shifted_binary_num HAS A length ITZ SUM OF binary_num'Z length AN num_bits
        FOUND YR shifted_binary_num
    IF U SAY SO


    OBTW Convert a binary number into a decimal number.
         The binary number must be provided in the format
         return by decimal_to_binary.
    TLDR
    HOW IZ I binary_to_decimal YR binary_num
        I HAS A length ITZ binary_num'Z length
        I HAS A decimal_num ITZ 0
        I HAS A i ITZ 0
        IM IN YR num_loop
            BOTH SAEM i AN length, O RLY?
                YA RLY, GTFO
            OIC
            I HAS A binary_value ITZ binary_num'Z SRS i
            I HAS A decimal_value ITZ 0
            BOTH SAEM binary_value AN 1, O RLY?
                YA RLY, decimal_value R I IZ power_of YR 2 AN YR i MKAY
            OIC
            decimal_num R SUM OF decimal_num AN decimal_value
            i R SUM OF i AN 1
        IM OUTTA YR num_loop
        FOUND YR decimal_num
    IF U SAY SO


    OBTW Equivalent to C's pow() function. Raises
         base to the power of exponent.
    TLDR
    HOW IZ I power_of YR base AN YR exponent
        I HAS A i ITZ 0
        I HAS A num ITZ 1
        IM IN YR num_loop
            BOTH SAEM i AN exponent, O RLY?
                YA RLY, GTFO
            OIC
            num R PRODUKT OF num AN base
            i R SUM OF i AN 1
        IM OUTTA YR num_loop
        FOUND YR num
    IF U SAY SO


    OBTW Return a binary number as a YARN.
         The binary number must be provided in the format
         return by decimal_to_binary.
    TLDR
    HOW IZ I binary_to_string YR binary_num
        I HAS A binary_string ITZ A YARN
        I HAS A i ITZ DIFF OF binary_num'Z length AN 1
        IM IN YR string_reverse
            DIFFRINT i AN BIGGR OF i AN 0, O RLY?
                YA RLY, GTFO
            OIC
            binary_string R SMOOSH binary_string AN binary_num'Z SRS i MKAY
            i R DIFF OF i AN 1
        IM OUTTA YR string_reverse
        FOUND YR binary_string
    IF U SAY SO

    OBTW Converts a hexadecimal number to the character
         equivalent in UNICODE. This was originally used
         in an attempt to write out to the P6 format of PPM,
         but the string produced by VISIBLE didn't seem to be
         properly formatted for some reason. Instead I fell back
         to P3 of PPM and wrote out to regular ascii format.
    TLDR
    HOW IZ I hex_to_char YR hex_string
        OBTW This is a hack I found for converting hexadecimal strings
             into their unicode character equivalents. Instead of using
             the ":" character directly, we escape it and get it using its
             unicode hex value (3A). This allows us to assemble the string
             with our inserted hex value without errors.
        TLDR
        FOUND YR SMOOSH ":(3A)" AN "(" AN hex_string AN ")" MKAY
    IF U SAY SO


    OBTW Equivalent to C's square() function. However it will only
         produce accurate results if the number is no larger than
         1048576. See the note below. This is based upon Newton's
         Approximation Method as adapted in C at this website (#11):

         http://www.codeproject.com/Articles/69941/Best-Square-Root-Method-Algorithm-Function-Precisi
    TLDR
    HOW IZ I square_rootin YR number
        OBTW Forcing a comparison between  the accuracy and a number
             which is so big that its precision is larger than the
             accuracy causes an infinite loop to occur. For this
             reason we have to set any number larger than 2^20 to
             a value below it.
        TLDR
        BOTH SAEM number AN BIGGR OF number AN 1048576.00, O RLY?
            YA RLY, number R 1048575.00
        OIC
        I HAS A accuracy ITZ 0.0001
        I HAS A lower ITZ A NUMBAR
        I HAS A upper ITZ A NUMBAR
        I HAS A guess ITZ A NUMBAR
        DIFFRINT number AN BIGGR OF number AN 1.0, O RLY?
            YA RLY
                lower R number
                upper R 1.0
            NO WAI
                lower R 1.0
                upper R number
        OIC
        IM IN YR LOOP
            I HAS A delta ITZ DIFF OF upper AN lower
            BOTH SAEM delta AN SMALLR OF delta AN accuracy, O RLY?
                YA RLY, GTFO
            OIC
            I HAS A guess ITZ QUOSHUNT OF SUM OF lower AN upper AN 2.0
            I HAS A guess_squared ITZ PRODUKT OF guess AN guess
            DIFFRINT guess_squared AN SMALLR OF guess_squared AN number, O RLY?
                YA RLY
                    upper R guess
                NO WAI
                    lower R guess
            OIC
        IM OUTTA YR LOOP
        FOUND YR QUOSHUNT OF SUM OF lower AN upper AN 2.0
    IF U SAY SO


    OBTW
         The intersection test for line [o, d]
         Return 2 if a hit was found (and also return distance t and bouncing ray n).
         Return 0 if no hit was found but ray goes upward
         Return 1 if no hit was found but ray goes downward
    TLDR
    HOW IZ I tracin YR o AN YR d
        I HAS A t ITZ 1000000000
        I HAS A m ITZ 0
        BOTH SAEM d'Z z AN 0, O RLY?
            YA RLY, d'Z z R 0.00001
        OIC
        I HAS A p ITZ QUOSHUNT OF DIFF OF 0 AN o'Z z AN d'Z z
        I HAS A n ITZ LIEK A Vector
        DIFFRINT p AN SMALLR OF p AN 0.01, O RLY?
            YA RLY
                t R p
                n R Vector IZ constructin YR 0.0 AN YR 0.0 AN YR 1.0 MKAY
                m R 1
        OIC

        BTW The world is encoded in sphere_positions, with 9 lines and 19 columns
        I HAS A k ITZ 18
        IM IN YR column_loop BTW For each column of objects
            BOTH SAEM k AN -1, O RLY?
                YA RLY, GTFO
            OIC

            I HAS A j ITZ 8
            IM IN YR line_loop BTW For each line on that column
                BOTH SAEM j AN -1, O RLY?
                    YA RLY, GTFO
                OIC

                I HAS A sphere_positions_line ITZ sphere_positions'Z SRS j
                sphere_positions_line'Z SRS k, O RLY?
                    YA RLY
                       BTW There is a sphere, but does the ray hit it?
                       p R Vector IZ addin YR o AN YR ...
                           Vector IZ constructin YR DIFF OF 0 AN k AN ...
                                                 YR 0 AN ...
                                                 YR DIFF OF DIFF OF 0 AN j AN 4 MKAY ...
                           MKAY
                       I HAS A b ITZ Vector IZ dot_productin YR p AN YR d MKAY
                       I HAS A q_c ITZ DIFF OF Vector IZ dot_productin YR p AN YR p MKAY AN 1
                       I HAS A q ITZ DIFF OF PRODUKT OF b AN b AN q_c


                       DIFFRINT q AN SMALLR OF q AN 0, O RLY?
                           YA RLY
                               BTW It does, compute the distance camera-sphere
                               I HAS A s ITZ DIFF OF DIFF OF 0 AN b AN I IZ square_rootin YR q MKAY


                               BOTH OF DIFFRINT s AN BIGGR OF s AN t AN ...
                                       DIFFRINT s AN SMALLR OF s AN 0.01, O RLY?
                                   YA RLY
                                       BTW So far this is the minimum distance, save it. And
                                       BTW also compute the bouncing ray vector into 'n'
                                       t R s
                                       I HAS A bouncing_ray ITZ Vector IZ scalin YR direction AN YR t MKAY
                                       bouncing_ray R Vector IZ addin YR p AN YR bouncing_ray MKAY
                                       n R Vector IZ normalizin YR bouncing_ray MKAY
                                       m R 2
                               OIC
                       OIC
                OIC
                j R DIFF OF j AN 1
            IM OUTTA YR line_loop
            k R DIFF OF k AN 1
        IM OUTTA YR column_loop
        I HAS A result ITZ A BUKKIT
        result HAS A m ITZ m
        result HAS A t ITZ t
        result HAS A n ITZ n
        FOUND YR result
    IF U SAY SO


    OBTW
        Sample the world and return the pixel color for
        a ray [o, d]
    TLDR
    HOW IZ I samplin YR o AN YR d

        BTW Search for an intersection ray Vs. world
        I HAS A result ITZ I IZ tracin YR o AN YR d MKAY
        I HAS A m ITZ result'Z m
        I HAS A t ITZ result'Z t
        I HAS A n ITZ result'Z n

        BOTH SAEM m AN 0, O RLY?
            YA RLY
                BTW No sphere found and the ray goes upward: Generate a sky color
                I HAS A vec_result ITZ Vector IZ constructin YR 0.7 AN YR 0.6 AN YR 1.0 MKAY

                I HAS A z_component ITZ d'Z z
                DIFFRINT z_component AN BIGGR OF z_component AN 0, O RLY?
                    YA RLY, z_component R 0
                OIC
                I HAS A vec_num ITZ DIFF OF 1 AN z_component
                vec_num R I IZ power_of YR vec_num AN YR 4 MKAY
                FOUND YR Vector IZ scalin YR vec_result AN YR vec_num MKAY
        OIC

        BTW h = intersection coordinate
        I HAS A h ITZ Vector IZ scalin YR d AN YR t MKAY
        h R Vector IZ addin YR o AN YR h MKAY
        BTW l = direction to light (with random delta for soft shadows)
        I HAS A l ITZ LIEK A Vector
        l HAS A x ITZ SUM OF 9 AN I IZ rand_onein MKAY
        l HAS A y ITZ SUM OF 9 AN I IZ rand_onein MKAY
        l HAS A z ITZ 16
        I HAS A l_two ITZ Vector IZ scalin YR h AN YR -1.0 MKAY
        l R Vector IZ addin YR l AN YR l_two MKAY
        l R Vector IZ normalizin YR l MKAY
        BTW r = The half-vector
        I HAS A r ITZ Vector IZ dot_productin YR n AN YR d MKAY
        r R PRODUKT OF r AN -2
        r R Vector IZ scalin YR n AN YR r MKAY
        r R Vector IZ addin YR d AN YR r MKAY

        BTW Calculate the lambertian factor
        I HAS A b ITZ Vector IZ dot_productin YR l AN YR n MKAY

        BTW Calculate illumination factor (lambertian coefficient > 0 or in shadow)?
        I HAS A illumination_result ITZ I IZ tracin YR h AN YR l MKAY
        I HAS A i_m ITZ illumination_result'Z m
        EITHER OF DIFFRINT b AN BIGGR OF b AN 0 AN BOTH SAEM i_m AN 2, O RLY?
            YA RLY, b R 0
        OIC

        BTW Calculate the color 'p' with diffuse and specular component
        I HAS A base
        DIFFRINT b AN SMALLR OF b AN 0, O RLY?
            YA RLY, base R 1
            NO WAI, base R 0
        OIC
        base R Vector IZ scalin YR r AN YR base MKAY
        base R Vector IZ dot_productin YR l AN YR r MKAY
        I HAS A p ITZ I IZ power_of YR base AN YR 99 MKAY

        BOTH SAEM m AN 1, O RLY?
            YA RLY
                BTW No sphere was hit and the ray was going downward: Generate a floor color
                h R Vector IZ scalin YR h AN YR 0.2 MKAY
                I HAS A ceil_h_x ITZ I IZ ceilin YR h'Z x MKAY
                I HAS A ceil_h_y ITZ I IZ ceilin YR h'Z y MKAY
                I HAS A ceil_h ITZ SUM OF ceil_h_x AN ceil_h_y
                ceil_h IS NOW A NUMBR
                I HAS A color_choice ITZ MOD OF ceil_h AN 2
                I HAS A color ITZ LIEK A Vector
                color_choice, O RLY?
                    YA RLY
                        color HAS A x ITZ 3
                        color HAS A y ITZ 1
                        color HAS A z ITZ 1
                    NO WAI
                        color HAS A x ITZ 3
                        color HAS A y ITZ 3
                        color HAS A z ITZ 3
                OIC
                FOUND YR Vector IZ scalin YR color AN YR SUM OF PRODUKT OF b AN 0.2 AN 0.1 MKAY
        OIC

        BTW m == 2 A sphere was hit. Cast a ray bouncing from the sphere surface.
        I HAS A sphere_color ITZ LIEK A Vector
        sphere_color HAS A x ITZ p
        sphere_color HAS A y ITZ p
        sphere_color HAS A z ITZ p
        I HAS A recursive_color ITZ I IZ samplin YR h AN YR r MKAY
        BTW Attenuate color by 50% since it is bouncing (* .5)
        recursive_color R Vector IZ scalin YR recursive_color AN YR 0.5 MKAY
        FOUND YR Vector IZ addin YR sphere_color AN YR recursive_color MKAY
    IF U SAY SO


    OBTW The vector class provides functionality for all the common
         linear algebra operations performed on vectors.
    TLDR
    O HAI IM Vector
        I HAS A x ITZ 0
        I HAS A y ITZ 0
        I HAS A z ITZ 0

        BTW Add vector_one and vector_two
        HOW IZ I addin YR vector_one AN YR vector_two
            I HAS A result ITZ LIEK A Vector
            result HAS A x ITZ 0
            result HAS A y ITZ 0
            result HAS A z ITZ 0
            result'Z x R SUM OF vector_one'Z x AN vector_two'Z x
            result'Z y R SUM OF vector_one'Z y AN vector_two'Z y
            result'Z z R SUM OF vector_one'Z z AN vector_two'Z z
            FOUND YR result
        IF U SAY SO

        BTW Scale vector_one by value
    HOW IZ I scalin YR vector_one AN YR value
            I HAS A result ITZ LIEK A Vector
            result HAS A x ITZ 0
            result HAS A y ITZ 0
            result HAS A z ITZ 0
            result'Z x R PRODUKT OF vector_one'Z x AN value
            result'Z y R PRODUKT OF vector_one'Z y AN value
            result'Z z R PRODUKT OF vector_one'Z z AN value
            FOUND YR result
        IF U SAY SO

        BTW Dot product of vector_one and vector_two
        HOW IZ I dot_productin YR vector_one AN YR vector_two
            FOUND YR SUM OF SUM OF PRODUKT OF vector_one'Z x AN vector_two'Z x AN ...
                                   PRODUKT OF vector_one'Z y AN vector_two'Z y AN ...
                                   PRODUKT OF vector_one'Z z AN vector_two'Z z
        IF U SAY SO

        BTW Cross product of vector_one and vector_two
        HOW IZ I cross_productin YR vector_one AN YR vector_two
        I HAS A result ITZ LIEK A Vector
            result HAS A x ITZ 0
            result HAS A y ITZ 0
            result HAS A z ITZ 0
            result'Z x R DIFF OF PRODUKT OF vector_one'Z y AN vector_two'Z z AN ...
                                 PRODUKT OF vector_one'Z z AN vector_two'Z y
            result'Z y R DIFF OF PRODUKT OF vector_one'Z z AN vector_two'Z x AN ...
                                 PRODUKT OF vector_one'Z x AN vector_two'Z z
            result'Z z R DIFF OF PRODUKT OF vector_one'Z x AN vector_two'Z y AN ...
                                 PRODUKT OF vector_one'Z y AN vector_two'Z x
            FOUND YR result
        IF U SAY SO

        BTW Length of vector_one
        HOW IZ I lengthin YR vector_one
            FOUND YR I IZ square_rootin YR ...
                SUM OF SUM OF PRODUKT OF vector_one'Z x AN vector_one'Z x AN ...
                              PRODUKT OF vector_one'Z y AN vector_one'Z y AN ...
                              PRODUKT OF vector_one'Z z AN vector_one'Z z MKAY
        IF U SAY SO

        BTW Normalize vector_one
        HOW IZ I normalizin YR vector_one
            I HAS A result ITZ LIEK A Vector
            result HAS A x ITZ 0
            result HAS A y ITZ 0
            result HAS A z ITZ 0
            I HAS A length ITZ Vector IZ lengthin YR vector_one MKAY
            BOTH SAEM length AN 0, O RLY?
                YA RLY
                    length R 1
            OIC
            result'Z x R QUOSHUNT OF vector_one'Z x AN length
            result'Z y R QUOSHUNT OF vector_one'Z y AN length
            result'Z z R QUOSHUNT OF vector_one'Z z AN length
            FOUND YR result
        IF U SAY SO

        BTW Printable YARN version of vector
        HOW IZ I to_stringin YR vector
            FOUND YR SMOOSH "[" AN vector'Z x AN ", " ...
                                AN vector'Z y AN ", " ...
                                AN vector'Z z AN "]" MKAY
        IF U SAY SO

        BTW Create and return a vector with components x, y, and z
        HOW IZ I constructin YR x AN YR y AN YR z
            I HAS A result ITZ LIEK A Vector
            result HAS A x ITZ x
            result HAS A y ITZ y
            result HAS A z ITZ z
            FOUND YR result
        IF U SAY SO
    KTHX

    OBTW The positions of the spheres are essentially
         stored in a 2-D array. This differs from Kensler's
         version where he used bit flags to store the
         positions in a compressed and quickly accessed
         manner. Unfortunately for us, bit operations
         in LOLCODE were too slow for this to be a tenable
         solution.
    TLDR
    I HAS A sphere_positions ITZ A BUKKIT
    I HAS A sphere_positions_0 ITZ A BUKKIT
    IM IN YR LOOP UPPIN YR pos_index TIL BOTH SAEM pos_index AN 19
        sphere_positions_0 HAS A SRS pos_index ITZ FAIL
    IM OUTTA YR LOOP
    sphere_positions HAS A SRS 0 ITZ sphere_positions_0
    I HAS A sphere_positions_1 ITZ A BUKKIT
    sphere_positions_1 HAS A SRS 0 ITZ WIN
    BTW sphere_positions_1 HAS A SRS 0 ITZ FAIL
    sphere_positions_1 HAS A SRS 1 ITZ FAIL
    sphere_positions_1 HAS A SRS 2 ITZ WIN
    BTW sphere_positions_1 HAS A SRS 2 ITZ FAIL
    sphere_positions_1 HAS A SRS 3 ITZ FAIL
    sphere_positions_1 HAS A SRS 4 ITZ WIN
    BTW sphere_positions_1 HAS A SRS 4 ITZ FAIL
    sphere_positions_1 HAS A SRS 5 ITZ FAIL
    sphere_positions_1 HAS A SRS 6 ITZ FAIL
    sphere_positions_1 HAS A SRS 7 ITZ WIN
    BTW sphere_positions_1 HAS A SRS 7 ITZ FAIL
    sphere_positions_1 HAS A SRS 8 ITZ FAIL
    sphere_positions_1 HAS A SRS 9 ITZ WIN
    BTW sphere_positions_1 HAS A SRS 9 ITZ FAIL
    sphere_positions_1 HAS A SRS 10 ITZ FAIL
    sphere_positions_1 HAS A SRS 11 ITZ WIN
    BTW sphere_positions_1 HAS A SRS 11 ITZ FAIL
    sphere_positions_1 HAS A SRS 12 ITZ FAIL
    sphere_positions_1 HAS A SRS 13 ITZ FAIL
    sphere_positions_1 HAS A SRS 14 ITZ WIN
    BTWsphere_positions_1 HAS A SRS 14 ITZ FAIL
    sphere_positions_1 HAS A SRS 15 ITZ FAIL
    sphere_positions_1 HAS A SRS 16 ITZ WIN
    BTW sphere_positions_1 HAS A SRS 16 ITZ FAIL
    sphere_positions_1 HAS A SRS 17 ITZ FAIL
    sphere_positions_1 HAS A SRS 18 ITZ WIN
    BTW sphere_positions_1 HAS A SRS 18 ITZ FAIL
    sphere_positions HAS A SRS 1 ITZ sphere_positions_1
    I HAS A sphere_positions_2 ITZ A BUKKIT
    IM IN YR LOOP UPPIN YR pos_index TIL BOTH SAEM pos_index AN 19
        sphere_positions_2 HAS A SRS pos_index ITZ FAIL
    IM OUTTA YR LOOP
    sphere_positions HAS A SRS 2 ITZ sphere_positions_2
    I HAS A sphere_positions_3 ITZ A BUKKIT
    sphere_positions_3 HAS A SRS 0 ITZ FAIL
    sphere_positions_3 HAS A SRS 1 ITZ FAIL
    sphere_positions_3 HAS A SRS 2 ITZ FAIL
    sphere_positions_3 HAS A SRS 3 ITZ FAIL
    sphere_positions_3 HAS A SRS 4 ITZ WIN
    BTW sphere_positions_3 HAS A SRS 4 ITZ FAIL
    sphere_positions_3 HAS A SRS 5 ITZ FAIL
    sphere_positions_3 HAS A SRS 6 ITZ FAIL
    sphere_positions_3 HAS A SRS 7 ITZ WIN
    BTW sphere_positions_3 HAS A SRS 7 ITZ FAIL
    sphere_positions_3 HAS A SRS 8 ITZ FAIL
    sphere_positions_3 HAS A SRS 9 ITZ FAIL
    sphere_positions_3 HAS A SRS 10 ITZ FAIL
    sphere_positions_3 HAS A SRS 11 ITZ WIN
    sphere_positions_3 HAS A SRS 12 ITZ FAIL
    sphere_positions_3 HAS A SRS 13 ITZ FAIL
    sphere_positions_3 HAS A SRS 14 ITZ FAIL
    sphere_positions_3 HAS A SRS 15 ITZ FAIL
    sphere_positions_3 HAS A SRS 16 ITZ FAIL
    sphere_positions_3 HAS A SRS 17 ITZ FAIL
    sphere_positions_3 HAS A SRS 18 ITZ WIN
    BTW sphere_positions_3 HAS A SRS 18 ITZ FAIL
    sphere_positions HAS A SRS 3 ITZ sphere_positions_3
    I HAS A sphere_positions_4 ITZ A BUKKIT
    IM IN YR LOOP UPPIN YR pos_index TIL BOTH SAEM pos_index AN 19
        sphere_positions_4 HAS A SRS pos_index ITZ FAIL
    IM OUTTA YR LOOP
    sphere_positions HAS A SRS 4 ITZ sphere_positions_4
    I HAS A sphere_positions_5 ITZ A BUKKIT
    sphere_positions_5 HAS A SRS 0 ITZ FAIL
    sphere_positions_5 HAS A SRS 1 ITZ FAIL
    sphere_positions_5 HAS A SRS 2 ITZ FAIL
    sphere_positions_5 HAS A SRS 3 ITZ FAIL
    sphere_positions_5 HAS A SRS 4 ITZ WIN
    BTW sphere_positions_5 HAS A SRS 4 ITZ FAIL
    sphere_positions_5 HAS A SRS 5 ITZ FAIL
    sphere_positions_5 HAS A SRS 6 ITZ FAIL
    sphere_positions_5 HAS A SRS 7 ITZ WIN
    BTW sphere_positions_5 HAS A SRS 7 ITZ FAIL
    sphere_positions_5 HAS A SRS 8 ITZ FAIL
    sphere_positions_5 HAS A SRS 9 ITZ WIN
    BTW sphere_positions_5 HAS A SRS 9 ITZ FAIL
    sphere_positions_5 HAS A SRS 10 ITZ FAIL
    sphere_positions_5 HAS A SRS 11 ITZ WIN
    BTW sphere_positions_5 HAS A SRS 11 ITZ FAIL
    sphere_positions_5 HAS A SRS 12 ITZ FAIL
    sphere_positions_5 HAS A SRS 13 ITZ FAIL
    sphere_positions_5 HAS A SRS 14 ITZ FAIL
    sphere_positions_5 HAS A SRS 15 ITZ FAIL
    sphere_positions_5 HAS A SRS 16 ITZ FAIL
    sphere_positions_5 HAS A SRS 17 ITZ FAIL
    sphere_positions_5 HAS A SRS 18 ITZ WIN
    BTW sphere_positions_5 HAS A SRS 18 ITZ FAIL
    sphere_positions HAS A SRS 5 ITZ sphere_positions_5
    I HAS A sphere_positions_6 ITZ A BUKKIT
    IM IN YR LOOP UPPIN YR pos_index TIL BOTH SAEM pos_index AN 19
        sphere_positions_6 HAS A SRS pos_index ITZ FAIL
    IM OUTTA YR LOOP
    sphere_positions HAS A SRS 6 ITZ sphere_positions_6
    I HAS A sphere_positions_7 ITZ A BUKKIT
    IM IN YR LOOP UPPIN YR pos_index TIL BOTH SAEM pos_index AN 19
        sphere_positions_7 HAS A SRS pos_index ITZ FAIL
    IM OUTTA YR LOOP
    sphere_positions HAS A SRS 7 ITZ sphere_positions_7
    I HAS A sphere_positions_8 ITZ A BUKKIT
    IM IN YR LOOP UPPIN YR pos_index TIL BOTH SAEM pos_index AN 19
        sphere_positions_8 HAS A SRS pos_index ITZ FAIL
    IM OUTTA YR LOOP
    sphere_positions HAS A SRS 8 ITZ sphere_positions_8

    BTW Camera direction
    I HAS A g ITZ Vector IZ constructin YR -6.0 AN YR -16.0 AN YR 0.0 MKAY
    g R Vector IZ normalizin YR g MKAY

    BTW Camera up vector
    I HAS A a ITZ Vector IZ constructin YR 0.0 AN YR 0.0 AN YR 1.0 MKAY
    a R Vector IZ cross_productin YR a AN YR g MKAY
    a R Vector IZ normalizin YR a MKAY
    a R Vector IZ scalin YR a AN YR 0.002 MKAY
    BTW Camera right vector
    I HAS A b ITZ Vector IZ cross_productin YR g AN YR a MKAY
    b R Vector IZ normalizin YR b MKAY
    b R Vector IZ scalin YR b AN YR 0.002 MKAY
    BTW Camera eye offset
    I HAS A c ITZ Vector IZ addin YR a AN YR b MKAY
    c R Vector IZ scalin YR c AN YR -256.0 MKAY
    c R Vector IZ addin YR c AN YR g MKAY

    I HAS A max_x ITZ 511
    I HAS A max_y ITZ max_x
    BTW Issue the PPM Header info
    VISIBLE "P3 " SUM OF max_x AN 1 " " SUM OF max_y AN 1 " 255"!

    I HAS A viewpoint ITZ Vector IZ constructin YR 17 AN YR 16 AN YR 8 MKAY

    I HAS A y ITZ max_y
    IM IN YR y_loop
        BOTH SAEM y AN -1, O RLY?
            YA RLY, GTFO
        OIC
        I HAS A x ITZ max_x
        IM IN YR x_loop
            BOTH SAEM x AN -1, O RLY?
                YA RLY, GTFO
            OIC
            I HAS A pixel_color ITZ Vector IZ constructin YR 13 AN YR 13 AN YR 13 MKAY

            I HAS A rays ITZ 64
            IM IN YR ray_loop
                BOTH SAEM rays AN 0, O RLY?
                    YA RLY, GTFO
                OIC

                BTW The delta to apply to the origin of the view (For Depth of View blur).
                I HAS A a_rand ITZ DIFF OF I IZ rand_onein MKAY AN 0.5
                I HAS A t_a ITZ Vector IZ scalin YR a AN YR a_rand MKAY
                t_a R Vector IZ scalin YR t_a AN YR 99.0 MKAY
                I HAS A b_rand ITZ DIFF OF I IZ rand_onein MKAY AN 0.5
                I HAS A t_b ITZ Vector IZ scalin YR b AN YR b_rand MKAY
                t_b R Vector IZ scalin YR t_b AN YR 99.0 MKAY
                I HAS A t ITZ Vector IZ addin YR t_a AN YR t_b MKAY

                I HAS A origin ITZ Vector IZ addin YR viewpoint AN YR t MKAY

                BTW Ray direction with random deltas for stochastic sampling
                I HAS A direction_up ITZ SUM OF I IZ rand_onein MKAY AN x
                direction_up R Vector IZ scalin YR a AN YR direction_up MKAY
                I HAS A direction_right ITZ SUM OF I IZ rand_onein MKAY AN y
                direction_right R Vector IZ scalin YR b AN YR direction_right MKAY
                I HAS A direction_t ITZ Vector IZ scalin YR t AN YR -1 MKAY
                I HAS A direction ITZ Vector IZ addin YR direction_right AN YR direction_up MKAY
                direction R Vector IZ addin YR direction AN YR c MKAY
                direction R Vector IZ scalin YR direction AN YR 16 MKAY
                direction R Vector IZ addin YR direction AN YR direction_t MKAY
                direction R Vector IZ normalizin YR direction MKAY

                I HAS A sample_color ITZ I IZ samplin YR origin AN YR direction MKAY
                sample_color R Vector IZ scalin YR sample_color AN YR 3.5 MKAY
                BTW + pixel_color for color accumulation
                pixel_color R Vector IZ addin YR sample_color AN YR pixel_color MKAY
                rays R DIFF OF rays AN 1
            IM OUTTA YR ray_loop
            I HAS A write_color ITZ pixel_color
            write_color'Z x IS NOW A NUMBR
            write_color'Z y IS NOW A NUMBR
            write_color'Z z IS NOW A NUMBR
            DIFFRINT write_color'Z x AN BIGGR OF write_color'Z x AN 0, O RLY?
                YA RLY, write_color'Z x R 0
            OIC
            DIFFRINT write_color'Z y AN BIGGR OF write_color'Z y AN 0, O RLY?
                YA RLY, write_color'Z y R 0
            OIC
            DIFFRINT write_color'Z z AN BIGGR OF write_color'Z z AN 0, O RLY?
                YA RLY, write_color'Z z R 0
            OIC
            VISIBLE " " write_color'Z x " " ...
                    " " write_color'Z y " " ...
                    " " write_color'Z z " "!
            x R DIFF OF x AN 1
        IM OUTTA YR x_loop
        y R DIFF OF y AN 1
    IM OUTTA YR y_loop

KTHXBYE
