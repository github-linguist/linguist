with Ada.Text_IO;
with Ada.Numerics.Elementary_Functions;
procedure Sundial is
   use Ada.Numerics.Elementary_Functions;
   use Ada.Numerics;
   package Float_IO is new Ada.Text_IO.Float_IO (Float);

   Latitude, Longitude, Meridian : Float;
   Latitude_Sine                 : Float;
begin
   Ada.Text_IO.Put ("Enter latitude:       ");
   Float_IO.Get (Latitude);
   Ada.Text_IO.Put ("Enter longitude:      ");
   Float_IO.Get (Longitude);
   Ada.Text_IO.Put ("Enter legal meridian: ");
   Float_IO.Get (Meridian);
   Ada.Text_IO.New_Line;

   Latitude_Sine := Sin (Latitude * Pi / 180.0);
   Ada.Text_IO.Put_Line
     ("   sine of latitude:" & Float'Image (Latitude_Sine));
   Ada.Text_IO.Put_Line
     ("   diff longitude:" & Float'Image (Longitude - Meridian));
   Ada.Text_IO.New_Line;

   Ada.Text_IO.Put_Line
     ("hour, sun hour angle, dial hour line angle from 6am to 6pm");
   for H in -6 .. 6 loop
      declare
         Hour_Angle : constant Float :=
            15.0 * Float (H) - (Longitude - Meridian);
         Line_Angle : constant Float :=
            Arctan (Latitude_Sine * Tan (Hour_Angle * Pi / 180.0)) * 180.0 /
            Pi;
      begin
         Ada.Text_IO.Put_Line
           ("HR=" &
            Integer'Image (H) &
            "; HRA=" &
            Float'Image (Hour_Angle) &
            "; HLA=" &
            Float'Image (Line_Angle));
      end;
   end loop;
end Sundial;
