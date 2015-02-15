with Glib; use Glib;
with Cairo; use Cairo;
with Cairo.Png; use Cairo.Png;
with Cairo.Image_Surface; use Cairo.Image_Surface;

procedure YinYang is
   subtype Dub is Glib.Gdouble;

   procedure Draw (C : Cairo_Context; x : Dub; y : Dub; r : Dub) is begin
      Arc (C, x, y, r + 1.0, 1.571, 7.854);
      Set_Source_Rgb (C, 0.0, 0.0, 0.0); Fill (C);
      Arc_Negative (C, x, y - r / 2.0, r / 2.0, 1.571, 4.712);
      Arc (C, x, y + r / 2.0, r / 2.0, 1.571, 4.712);
      Arc_Negative (C, x, y, r, 4.712, 1.571);
      Set_Source_Rgb (C, 1.0, 1.0, 1.0); Fill (C);
      Arc (C, x, y - r / 2.0, r / 5.0, 1.571, 7.854);
      Set_Source_Rgb (C, 0.0, 0.0, 0.0); Fill (C);
      Arc (C, x, y + r / 2.0, r / 5.0, 1.571, 7.854);
      Set_Source_Rgb (C, 1.0, 1.0, 1.0); Fill (C);
   end Draw;

   Surface : Cairo_Surface;
   Context : Cairo_Context;
   Status : Cairo_Status;
begin
   Surface := Create (Cairo_Format_ARGB32, 200, 200);
   Context := Create (Surface);
   Draw (Context, 120.0, 120.0, 75.0);
   Draw (Context, 35.0, 35.0, 30.0);
   Status := Write_To_Png (Surface, "YinYangAda.png");
   pragma Assert (Status = Cairo_Status_Success);
end YinYang;
