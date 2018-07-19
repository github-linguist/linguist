with Ada.Text_IO;  use Ada.Text_IO;

procedure Test_Common_Path is
   function "rem" (A, B : String) return String is
      Slash : Integer := A'First; -- At the last slash seen in A
      At_A  : Integer := A'first;
      At_B  : Integer := B'first;
   begin
      loop
         if At_A > A'Last then
            if At_B > B'Last or else B (At_B) = '/' then
               return A;
            else
               return A (A'First..Slash - 1);
            end if;
         elsif At_B > B'Last then
            if A (At_A) = '/' then -- A cannot be shorter than B here
               return B;
            else
               return A (A'First..Slash - 1);
            end if;
         elsif A (At_A) /= B (At_B) then
            return A (A'First..Slash - 1);
         elsif A (At_A) = '/' then
            Slash := At_A;
         end if;
         At_A := At_A + 1;
         At_B := At_B + 1;
      end loop;
   end "rem";
begin
   Put_Line
   (  "/home/user1/tmp/coverage/test" rem
      "/home/user1/tmp/covert/operator" rem
      "/home/user1/tmp/coven/members"
   );
end Test_Common_Path;
