with Ada.Text_IO;

procedure Vignere_Cryptanalysis is

   subtype Letter is Character range 'A' .. 'Z';

   function "+"(X, Y: Letter) return Letter is
   begin
      return Character'Val( ( (Character'Pos(X)-Character'Pos('A'))
                                + (Character'Pos(Y)-Character'Pos('A')) ) mod 26
                          + Character'Pos('A'));
   end;

   function "-"(X, Y: Letter) return Letter is
   begin
      return Character'Val( ( (Character'Pos(X)-Character'Pos('A'))
                                - (Character'Pos(Y)-Character'Pos('A')) ) mod 26
                          + Character'Pos('A'));
   end;

   type Frequency_Array is array (Letter) of Float;

   English: Frequency_Array :=
     ( 0.08167, 0.01492, 0.02782, 0.04253, 0.12702, 0.02228, 0.02015,
       0.06094, 0.06966, 0.00153, 0.00772, 0.04025, 0.02406, 0.06749,
       0.07507, 0.01929, 0.00095, 0.05987, 0.06327, 0.09056, 0.02758,
       0.00978, 0.02360, 0.00150, 0.01974, 0.00074 );

   function Get_Frequency(S: String) return Frequency_Array is
      Result: Frequency_Array := (others => 0.0);
      Offset: Float := 1.0/Float(S'Length);
   begin
      for I in S'Range loop
         if S(I) in Letter then
            Result(S(I)) := Result(S(I)) + Offset;
         end if;
      end loop;
      return Result;
   end Get_Frequency;

   function Remove_Whitespace(S: String) return String is
   begin
      if S="" then
         return "";
      elsif S(S'First) in Letter then
         return S(S'First) & Remove_Whitespace(S(S'First+1 .. S'Last));
      else
         return Remove_Whitespace(S(S'First+1 .. S'Last));
      end if;
   end Remove_Whitespace;

   function Distance(A, B: Frequency_Array;
                     Offset: Character := 'A') return Float is
      Result: Float := 0.0;
      Diff: Float;
   begin
      for C in A'Range loop
         Diff := A(C+Offset) - B(C);
         Result := Result + (Diff * Diff);
      end loop;
      return Result;
   end Distance;

   function Find_Key(Cryptogram: String; Key_Length: Positive) return String is

      function Find_Caesar_Key(S: String) return Letter is
         Frequency: Frequency_Array := Get_Frequency(S);
         Candidate: Letter := 'A'; -- a fake candidate
         Candidate_Dist : Float := Distance(Frequency, English, 'A');
         New_Dist: Float;

      begin

         for L in Letter range 'B' .. 'Z' loop
            New_Dist := Distance(Frequency, English, L);
            if New_Dist <= Candidate_Dist then
               Candidate_Dist := New_Dist;
               Candidate      := L;
            end if;
         end loop;
         return Candidate;
      end Find_Caesar_Key;

      function Get_Slide(S: String; Step: Positive) return String is
      begin
         if S'Length= 0 then
            return "";
         else
            return S(S'First) & Get_Slide(S(S'First+Step .. S'Last), Step);
         end if;
      end Get_Slide;

      Key: String(1 .. Key_Length);

      S: String renames Cryptogram;

   begin
      for I in Key'Range loop
         Key(I) := Find_Caesar_Key(Get_Slide(S(S'First+I-1 .. S'Last),
                                             Key_Length));
      end loop;
      return Key;
   end Find_Key;

   function Key_Char(Key: String; Index: Positive) return Letter is
   begin
      if Index > Key'Last then
         return Key_Char(Key, Index-Key'Last);
      else
         return Key(Index);
      end if;
   end Key_Char;

   Ciphertext: String := Remove_Whitespace(
     "MOMUD EKAPV TQEFM OEVHP AJMII CDCTI FGYAG JSPXY ALUYM NSMYH" &
     "VUXJE LEPXJ FXGCM JHKDZ RYICU HYPUS PGIGM OIYHF WHTCQ KMLRD" &
     "ITLXZ LJFVQ GHOLW CUHLO MDSOE KTALU VYLNZ RFGBX PHVGA LWQIS" &
     "FGRPH JOOFW GUBYI LAPLA LCAFA AMKLG CETDW VOELJ IKGJB XPHVG" &
     "ALWQC SNWBU BYHCU HKOCE XJEYK BQKVY KIIEH GRLGH XEOLW AWFOJ" &
     "ILOVV RHPKD WIHKN ATUHN VRYAQ DIVHX FHRZV QWMWV LGSHN NLVZS" &
     "JLAKI FHXUF XJLXM TBLQV RXXHR FZXGV LRAJI EXPRV OSMNP KEPDT" &
     "LPRWM JAZPK LQUZA ALGZX GVLKL GJTUI ITDSU REZXJ ERXZS HMPST" &
     "MTEOE PAPJH SMFNB YVQUZ AALGA YDNMP AQOWT UHDBV TSMUE UIMVH" &
     "QGVRW AEFSP EMPVE PKXZY WLKJA GWALT VYYOB YIXOK IHPDS EVLEV" &
     "RVSGB JOGYW FHKBL GLXYA MVKIS KIEHY IMAPX UOISK PVAGN MZHPW" &
     "TTZPV XFCCD TUHJH WLAPF YULTB UXJLN SIJVV YOVDJ SOLXG TGRVO" &
     "SFRII CTMKO JFCQF KTINQ BWVHG TENLH HOGCS PSFPV GJOKM SIFPR" &
     "ZPAAS ATPTZ FTPPD PORRF TAXZP KALQA WMIUD BWNCT LEFKO ZQDLX" &
     "BUXJL ASIMR PNMBF ZCYLV WAPVF QRHZV ZGZEF KBYIO OFXYE VOWGB" &
     "BXVCB XBAWG LQKCM ICRRX MACUO IKHQU AJEGL OIJHH XPVZW JEWBA" &
     "FWAML ZZRXJ EKAHV FASMU LVVUT TGK");

   Best_Plain: String := Ciphertext;
   Best_Dist:  Float := Distance(English, Get_Frequency(Best_Plain));
   Best_Key:   String := Ciphertext;
   Best_Key_L: Natural := 0;

begin -- Vignere_Cryptanalysis
   for I in 1 .. Ciphertext'Length/10 loop
      declare
         Key: String(1 .. I) := Find_Key(Ciphertext, I);
         Plaintext: String(Ciphertext'Range);
      begin
         for I in Ciphertext'Range loop
            Plaintext(I) := Ciphertext(I) - Key_Char(Key, I);
         end loop;
         if Distance(English, Get_Frequency(Plaintext)) < Best_Dist then
            Best_Plain := Plaintext;
            Best_Dist  := Distance(English, Get_Frequency(Plaintext));
            Best_Key(1 .. I) := Key;
            Best_Key_L := I;
            if Best_dist < 0.01 then
               declare
                  use Ada.Text_IO;
               begin
                  Put_Line("Key       =" & Best_Key(1 .. Best_Key_L));
                  Put_Line("Distance = " & Float'Image(Best_Dist));
                  New_Line;
                  Put_Line("Plaintext =");
                  Put_Line(Best_Plain);
                  New_Line; New_Line;
               end;
            end if;
         end if;
      end;
   end loop;
end Vignere_Cryptanalysis;
