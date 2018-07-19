with Ada.Text_IO, Ada.Containers.Ordered_Sets;

procedure Set_Demo is

   package CS is new  Ada.Containers.Ordered_Sets(Character); use CS;
   package IO renames Ada.Text_IO;

   -- helper functions for string to something conversion, and vice versa
        function To_Set(S: String) return Set is
           Result: Set;
        begin
           for I in S'Range loop
              begin
                 Result.Insert(S(I));
                 -- raises Constraint_Error if S(I) is already in Result
              exception
                 when Constraint_Error => null;
              end;
           end loop;
           return Result;
        end To_Set;

        function Image(S: Set) return String is
           C: Character;
           T: Set := S;
        begin
           if T.Is_Empty then
              return "";
           else
              C:= T.First_Element;
              T.Delete_First;
              return C & Image(T);
           end if;
        end Image;

        function Image(C: Ada.Containers.Count_Type) return String renames
          Ada.Containers.Count_Type'Image;

   S1, S2: Set;
begin -- main program
   loop
      S1 := To_Set(Ada.Text_IO.Get_Line);
      exit when S1 = To_Set("quit!");
      S2 := To_Set(Ada.Text_IO.Get_Line);
      IO.Put_Line("Sets [" & Image(S1) & "], [" & Image(S2) & "] of size"
                    & Image(S1.Length) & " and" & Image(S2.Length) & ".");
      IO.Put_Line("Intersection:   [" & Image(Intersection(S1, S2)) & "],");
      IO.Put_Line("Union:          [" & Image(Union(S1, S2))        & "],");
      IO.Put_Line("Difference:     [" & Image(Difference(S1, S2))   & "],");
      IO.Put_Line("Symmetric Diff: [" & Image(S1 xor S2) & "],");
      IO.Put_Line("Subset: "  & Boolean'Image(S1.Is_Subset(S2))
                  & ", Equal: " & Boolean'Image(S1 = S2) & ".");
   end loop;
end Set_Demo;
