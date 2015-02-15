with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
procedure SelfRef is
   subtype Seed is Natural range 0 .. 1_000_000;
   subtype Num is Natural range 0 .. 10;
   type NumList is array (0 .. 10) of Num;
   package IO is new Ada.Text_IO.Integer_IO (Natural);
   package DVect is new Ada.Containers.Vectors (Positive, NumList);

   function Init (innum : Seed) return NumList is
      list : NumList := (others => 0);
      number : Seed := innum;  d : Num;
   begin
      loop
         d := Num (number mod 10);
         list (d) :=  list (d) + 1;
         number := number / 10; exit when number = 0;
      end loop; return list;
   end Init;

   procedure Next (inoutlist : in out NumList) is
      list : NumList := (others => 0);
   begin
      for i in list'Range loop
         if inoutlist (i) /= 0 then
            list (i) := list (i) + 1;
            list (inoutlist (i)) := list (inoutlist (i)) + 1;
         end if;
      end loop; inoutlist := list;
   end Next;

   procedure Show (list : NumList) is begin
      for i in reverse list'Range loop
         if list (i) > 0 then
            IO.Put (list (i), Width => 1); IO.Put (i, Width => 1);
         end if;
      end loop; New_Line;
   end Show;

   function Iterate (theseed : Seed; p : Boolean) return Natural is
      list : NumList := Init (theseed);
      vect : DVect.Vector;
   begin
      vect.Append (list);
      loop
         if p then Show (list); end if;
         Next (list); exit when vect.Contains (list); vect.Append (list);
      end loop;
      return Integer (DVect.Length (vect)) + 1;
   end Iterate;

   mseed : Seed;
   len, maxlen : Natural := 0;
begin
   for i in Seed'Range loop
      len := Iterate (i, False);
      if len > maxlen then mseed := i; maxlen := len; end if;
   end loop;
   IO.Put (maxlen, Width => 1); Put_Line (" Iterations:");
   IO.Put (mseed, Width => 1); New_Line;
   len := Iterate (mseed, True);
end SelfRef;
