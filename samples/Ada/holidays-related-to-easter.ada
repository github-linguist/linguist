with Ada.Calendar.Arithmetic;
with Ada.Calendar.Formatting;
with Ada.Text_IO;

procedure Main is
   use Ada.Calendar;
   use Arithmetic;

   function Get_Easter (Year : Year_Number) return Time is
      A : Integer := Year mod 19;
      B : Integer := Year / 100;
      C : Integer := Year mod 100;
      D : Integer := B / 4;
      E : Integer := B mod 4;
      F : Integer := (B + 8) / 25;
      G : Integer := (B - F + 1) / 3;
      H : Integer := (19 * A + B - D - G + 15) mod 30;
      I : Integer := C / 4;
      K : Integer := C mod 4;
      L : Integer := (32 + 2 * E + 2 * I - H - K) mod 7;
      M : Integer := (A + 11 * H + 22 * L) / 451;
      N : Integer := H + L - 7 * M + 114;
   begin
      return Time_Of (Year, N / 31, N mod 31 + 1, 43200.0);
   end Get_Easter;

   procedure Print_Easter (Year : Year_Number) is
      Days_To_Ascension : constant Day_Count := 39;
      Days_To_Pentecost : constant Day_Count := 49;
      Days_To_Trinity   : constant Day_Count := 56;
      Days_To_Corpus    : constant Day_Count := 60;
      Easter : Time := Get_Easter (Year);
   begin
      Ada.Text_IO.Put (Integer'Image (Year));
      Ada.Text_IO.Put (": Easter: " &
                       Formatting.Image (Easter) (6 .. 10));
      Ada.Text_IO.Put (", Ascension: " &
                       Formatting.Image (Easter + Days_To_Ascension) (6 .. 10));
      Ada.Text_IO.Put (", Pentecost: " &
                       Formatting.Image (Easter + Days_To_Pentecost) (6 .. 10));
      Ada.Text_IO.Put (", Trinity: " &
                       Formatting.Image (Easter + Days_To_Trinity) (6 .. 10));
      Ada.Text_IO.Put_Line (", Corpus: " &
                       Formatting.Image (Easter + Days_To_Corpus) (6 .. 10));
   end Print_Easter;
begin
   Ada.Text_IO.Put_Line
     ("Christian holidays, related to Easter, for years from 2010 to 2020 CE:");
   for I in 2010 .. 2020 loop
      Print_Easter (I);
   end loop;
end Main;
