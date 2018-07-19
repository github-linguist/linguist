with Ada.Text_IO; use Ada.Text_IO;
procedure Zebra is
   type Content is (Beer, Coffee, Milk, Tea, Water,
      Danish, English, German, Norwegian, Swedish,
      Blue, Green, Red, White, Yellow,
      Blend, BlueMaster, Dunhill, PallMall, Prince,
      Bird, Cat, Dog, Horse, Zebra);
   type Test is (Drink, Person, Color, Smoke, Pet);
   type House is (One, Two, Three, Four, Five);
   type Street is array (Test'Range, House'Range) of Content;
   type Alley is access all Street;

   procedure Print (mat : Alley) is begin
      for H in House'Range loop
         Put(H'Img&": ");
         for T in Test'Range loop
            Put(T'Img&"="&mat(T,H)'Img&" ");
      end loop; New_Line; end loop;
   end Print;

   function FinalChecks (mat : Alley) return Boolean is
      function Diff (A, B : Content; CA , CB : Test) return Integer is begin
         for H1 in House'Range loop for H2 in House'Range loop
               if mat(CA,H1) = A and mat(CB,H2) = B then
                  return House'Pos(H1) - House'Pos(H2);
               end if;
         end loop; end loop;
      end Diff;
   begin
      if abs(Diff(Norwegian, Blue, Person, Color)) = 1
        and Diff(Green, White, Color, Color) = -1
        and abs(Diff(Horse, Dunhill, Pet, Smoke)) = 1
        and abs(Diff(Water, Blend, Drink, Smoke)) = 1
        and abs(Diff(Blend, Cat, Smoke, Pet)) = 1
      then return True;
      end if;
      return False;
   end FinalChecks;

   function Constrained (mat : Alley; atest : Natural) return Boolean is begin
      --  Tests seperated into levels for speed, not strictly necessary
      --  As such, the program finishes in around ~0.02s
      case Test'Val (atest) is
         when Drink => --  Drink
            if mat (Drink, Three) /= Milk then return False; end if;
            return True;
         when Person => --  Drink+Person
            for H in House'Range loop
               if (mat(Person,H) = Norwegian and H /= One)
               or (mat(Person,H) = Danish and mat(Drink,H) /= Tea)
               then return False; end if;
            end loop;
            return True;
         when Color => --  Drink+People+Color
            for H in House'Range loop
               if (mat(Person,H) = English and mat(Color,H) /= Red)
               or (mat(Drink,H) = Coffee and mat(Color,H) /= Green)
               then return False; end if;
            end loop;
            return True;
         when Smoke => --  Drink+People+Color+Smoke
            for H in House'Range loop
               if (mat(Color,H) = Yellow and mat(Smoke,H) /= Dunhill)
               or (mat(Smoke,H) = BlueMaster and mat(Drink,H) /= Beer)
               or (mat(Person,H) = German and mat(Smoke,H) /= Prince)
               then return False; end if;
            end loop;
            return True;
         when Pet => --  Drink+People+Color+Smoke+Pet
            for H in House'Range loop
               if (mat(Person,H) = Swedish and mat(Pet,H) /= Dog)
               or (mat(Smoke,H) = PallMall and mat(Pet,H) /= Bird)
               then return False; end if;
            end loop;
            return FinalChecks(mat); --  Do the next-to checks
      end case;
   end Constrained;

   procedure Solve (mat : Alley; t, n : Natural) is
      procedure Swap (I, J : Natural) is
         temp : constant Content := mat (Test'Val (t), House'Val (J));
      begin
         mat (Test'Val (t), House'Val (J)) := mat (Test'Val (t), House'Val (I));
         mat (Test'Val (t), House'Val (I)) := temp;
      end Swap;
   begin
      if n = 1 and Constrained (mat, t) then --  test t passed
         if t < 4 then Solve (mat, t + 1, 5); --  Onto next test
         else Print (mat); return; --  Passed and t=4 means a solution
         end if;
      end if;
      for i in 0 .. n - 1 loop --  The permutations part
         Solve (mat, t, n - 1);
         if n mod 2 = 1 then Swap (0, n - 1);
         else Swap (i, n - 1); end if;
      end loop;
   end Solve;

   myStreet : aliased Street;
   myAlley : constant Alley := myStreet'Access;
begin
   for i in Test'Range loop for j in House'Range loop --  Init Matrix
      myStreet (i,j) := Content'Val(Test'Pos(i)*5 + House'Pos(j));
   end loop; end loop;
   Solve (myAlley, 0, 5); --  start at test 0 with 5 options
end Zebra;
