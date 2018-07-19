with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Doubly_Linked_Lists;

procedure Pythagore_Set is

   type Triangles is array (1 .. 3) of Positive;

   package Triangle_Lists is new Ada.Containers.Doubly_Linked_Lists (
      Triangles);
   use Triangle_Lists;

   function Find_List (Upper_Bound : Positive) return List is
      L : List := Empty_List;
   begin
      for A in 1 .. Upper_Bound loop
         for B in A + 1 .. Upper_Bound loop
            for C in B + 1 .. Upper_Bound loop
               if ((A * A + B * B) = C * C) then
                  Append (L, (A, B, C));
               end if;
            end loop;
         end loop;
      end loop;
      return L;
   end Find_List;

   Triangle_List : List;
   C             : Cursor;
   T             : Triangles;

begin
   Triangle_List := Find_List (Upper_Bound => 20);

   C := First (Triangle_List);
   while Has_Element (C) loop
      T := Element (C);
      Put
        ("(" &
         Integer'Image (T (1)) &
         Integer'Image (T (2)) &
         Integer'Image (T (3)) &
         ") ");
      Next (C);
   end loop;
end Pythagore_Set;
