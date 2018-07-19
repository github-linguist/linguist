with Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
procedure Circle is
   -- extreme coordinate values are -15:0, 15:0, 0:-15, 0:15
   subtype Coordinate is Integer range -15 .. 15;
   type Point is record
      X, Y : Coordinate;
   end record;
   type Point_List is array (Positive range <>) of Point;

   function Acceptable (Position : Point) return Boolean is
      Squared_Sum : Natural := Position.X ** 2 + Position.Y ** 2;
   begin
      return 10 ** 2 <= Squared_Sum and Squared_Sum <= 15 ** 2;
   end Acceptable;

   -- first algorithm
   function Generate_Random_Points
     (Count : Positive := 100)
      return  Point_List
   is
      package RNG is new Ada.Numerics.Discrete_Random (Coordinate);
      Generator  : RNG.Generator;
      Next_Point : Point;
      Result     : Point_List (1 .. Count);
   begin
      RNG.Reset (Generator);
      for N in Result'Range loop
         loop
            Next_Point.X := RNG.Random (Generator);
            Next_Point.Y := RNG.Random (Generator);
            exit when Acceptable (Next_Point);
         end loop;
         Result (N) := Next_Point;
      end loop;
      return Result;
   end Generate_Random_Points;

   -- second algorithm
   function Choose_Precalculated
     (Count : Positive := 100)
      return  Point_List
   is
      subtype Possible_Points is Positive range 1 .. 404;
      package RNG is new Ada.Numerics.Discrete_Random (Possible_Points);
      Generator  : RNG.Generator;
      Point_Pool : Point_List (Possible_Points);
      Next_Point : Point;
      Next_Index : Possible_Points := 1;
      Result     : Point_List (1 .. Count);
   begin
      -- precalculate
      Precalculate : for X in Coordinate'Range loop
         Next_Point.X := X;
         for Y in Coordinate'Range loop
            Next_Point.Y := Y;
            if Acceptable (Next_Point) then
               Point_Pool (Next_Index) := Next_Point;
               exit Precalculate when Next_Index = Possible_Points'Last;
               Next_Index := Next_Index + 1;
            end if;
         end loop;
      end loop Precalculate;
      -- choose
      RNG.Reset (Generator);
      for N in Result'Range loop
         Result (N) := Point_Pool (RNG.Random (Generator));
      end loop;
      return Result;
   end Choose_Precalculated;

   procedure Print_Points (Points : Point_List) is
      Output_String : array (Coordinate, Coordinate) of Character :=
        (others => (others => ' '));
   begin
      for N in Points'Range loop
         Output_String (Points (N).X, Points (N).Y) := '*';
      end loop;
      for Line in Output_String'Range (2) loop
         for Column in Output_String'Range (1) loop
            Ada.Text_IO.Put (Output_String (Column, Line));
         end loop;
         Ada.Text_IO.New_Line;
      end loop;
   end Print_Points;

   My_Circle_Randomly      : Point_List := Generate_Random_Points;
   My_Circle_Precalculated : Point_List := Choose_Precalculated;
begin
   Ada.Text_IO.Put_Line ("Randomly generated:");
   Print_Points (My_Circle_Randomly);
   Ada.Text_IO.Put_Line ("Chosen from precalculated:");
   Print_Points (My_Circle_Precalculated);
end Circle;
