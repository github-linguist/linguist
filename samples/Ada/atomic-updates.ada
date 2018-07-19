with Ada.Text_IO;  use Ada.Text_IO;
with Ada.Numerics.Discrete_Random;

procedure Test_Updates is

   type Bucket_Index is range 1..13;
   package Random_Index is new Ada.Numerics.Discrete_Random (Bucket_Index);
   use Random_Index;
   type Buckets is array (Bucket_Index) of Natural;

   protected type Safe_Buckets is
      procedure Initialize (Value : Buckets);
      function Get (I : Bucket_Index) return Natural;
      procedure Transfer (I, J : Bucket_Index; Amount : Integer);
      function Snapshot return Buckets;
   private
      Data : Buckets := (others => 0);
   end Safe_Buckets;

   protected body Safe_Buckets is
      procedure Initialize (Value : Buckets) is
      begin
         Data := Value;
      end Initialize;

      function Get (I : Bucket_Index) return Natural is
      begin
         return Data (I);
      end Get;

      procedure Transfer (I, J : Bucket_Index; Amount : Integer) is
         Increment : constant Integer :=
            Integer'Max (-Data (J), Integer'Min (Data (I), Amount));
      begin
         Data (I) := Data (I) - Increment;
         Data (J) := Data (J) + Increment;
      end Transfer;

      function Snapshot return Buckets is
      begin
         return Data;
      end Snapshot;
   end Safe_Buckets;

   Data : Safe_Buckets;

   task Equalize;
   task Mess_Up;

   task body Equalize is
      Dice : Generator;
      I, J : Bucket_Index;
   begin
      loop
         I := Random (Dice);
         J := Random (Dice);
         Data.Transfer (I, J, (Data.Get (I) - Data.Get (J)) / 2);
      end loop;
   end Equalize;

   task body Mess_Up is
      Dice : Generator;
   begin
      loop
         Data.Transfer (Random (Dice), Random (Dice), 100);
      end loop;
   end Mess_Up;

begin
   Data.Initialize ((1,2,3,4,5,6,7,8,9,10,11,12,13));
   loop
      delay 1.0;
      declare
         State : Buckets := Data.Snapshot;
         Sum   : Natural := 0;
      begin
         for Index in State'Range loop
            Sum := Sum + State (Index);
            Put (Integer'Image (State (Index)));
         end loop;
         Put (" =" & Integer'Image (Sum));
         New_Line;
      end;
   end loop;
end Test_Updates;
