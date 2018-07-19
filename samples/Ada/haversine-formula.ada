with Ada.Text_IO; use Ada.Text_IO;
with Ada.Long_Float_Text_IO; use Ada.Long_Float_Text_IO;
with Ada.Numerics.Generic_Elementary_Functions;

procedure Haversine_Formula is

   package Math is new Ada.Numerics.Generic_Elementary_Functions (Long_Float); use Math;

   -- Compute great circle distance, given latitude and longitude of two points, in radians
   function Great_Circle_Distance (lat1, long1, lat2, long2 : Long_Float) return Long_Float is
      Earth_Radius : constant := 6371.0; -- in kilometers
      a : Long_Float := Sin (0.5 * (lat2 - lat1));
      b : Long_Float := Sin (0.5 * (long2 - long1));
   begin
      return 2.0 * Earth_Radius * ArcSin (Sqrt (a * a + Cos (lat1) * Cos (lat2) * b * b));
   end Great_Circle_Distance;

   -- convert degrees, minutes and seconds to radians
   function DMS_To_Radians (Deg, Min, Sec : Long_Float := 0.0) return Long_Float is
      Pi_Over_180 : constant := 0.017453_292519_943295_769236_907684_886127;
   begin
      return (Deg + Min/60.0 + Sec/3600.0) * Pi_Over_180;
   end DMS_To_Radians;

begin
   Put_Line("Distance in kilometers between BNA and LAX");
   Put (Great_Circle_Distance (
         DMS_To_Radians (36.0, 7.2), DMS_To_Radians (86.0, 40.2),       -- Nashville International Airport (BNA)
         DMS_To_Radians (33.0, 56.4), DMS_To_Radians (118.0, 24.0)),    -- Los Angeles International Airport (LAX)
      Aft=>3, Exp=>0);
end Haversine_Formula;
