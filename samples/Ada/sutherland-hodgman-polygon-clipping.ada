with Ada.Containers.Doubly_Linked_Lists;
with Ada.Text_IO;

procedure Main is
   package FIO is new Ada.Text_IO.Float_IO (Float);

   type Point is record
      X, Y : Float;
   end record;

   function "-" (Left, Right : Point) return Point is
   begin
      return (Left.X - Right.X, Left.Y - Right.Y);
   end "-";

   type Edge is array (1 .. 2) of Point;

   package Point_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Point);
   use type Point_Lists.List;
   subtype Polygon is Point_Lists.List;

   function Inside (P : Point; E : Edge) return Boolean is
   begin
      return (E (2).X - E (1).X) * (P.Y - E (1).Y) >
             (E (2).Y - E (1).Y) * (P.X - E (1).X);
   end Inside;

   function Intersecton (P1, P2 : Point; E : Edge) return Point is
      DE : Point := E (1) - E (2);
      DP : Point := P1 - P2;
      N1 : Float := E (1).X * E (2).Y - E (1).Y * E (2).X;
      N2 : Float := P1.X * P2.Y - P1.Y * P2.X;
      N3 : Float := 1.0 / (DE.X * DP.Y - DE.Y * DP.X);
   begin
      return ((N1 * DP.X - N2 * DE.X) * N3, (N1 * DP.Y - N2 * DE.Y) * N3);
   end Intersecton;

   function Clip (P, C : Polygon) return Polygon is
      use Point_Lists;
      A, B, S, E : Cursor;
      Inputlist  : List;
      Outputlist : List := P;
      AB         : Edge;
   begin
      A := C.First;
      B := C.Last;
      while A /= No_Element loop
         AB        := (Element (B), Element (A));
         Inputlist := Outputlist;
         Outputlist.Clear;
         S := Inputlist.Last;
         E := Inputlist.First;
         while E /= No_Element loop
            if Inside (Element (E), AB) then
               if not Inside (Element (S), AB) then
                  Outputlist.Append
                    (Intersecton (Element (S), Element (E), AB));
               end if;
               Outputlist.Append (Element (E));
            elsif Inside (Element (S), AB) then
               Outputlist.Append
                 (Intersecton (Element (S), Element (E), AB));
            end if;
            S := E;
            E := Next (E);
         end loop;
         B := A;
         A := Next (A);
      end loop;
      return Outputlist;
   end Clip;

   procedure Print (P : Polygon) is
      use Point_Lists;
      C : Cursor := P.First;
   begin
      Ada.Text_IO.Put_Line ("{");
      while C /= No_Element loop
         Ada.Text_IO.Put (" (");
         FIO.Put (Element (C).X, Exp => 0);
         Ada.Text_IO.Put (',');
         FIO.Put (Element (C).Y, Exp => 0);
         Ada.Text_IO.Put (')');
         C := Next (C);
         if C /= No_Element then
            Ada.Text_IO.Put (',');
         end if;
         Ada.Text_IO.New_Line;
      end loop;
      Ada.Text_IO.Put_Line ("}");
   end Print;

   Source  : Polygon;
   Clipper : Polygon;
   Result  : Polygon;
begin
   Source.Append ((50.0, 150.0));
   Source.Append ((200.0, 50.0));
   Source.Append ((350.0, 150.0));
   Source.Append ((350.0, 300.0));
   Source.Append ((250.0, 300.0));
   Source.Append ((200.0, 250.0));
   Source.Append ((150.0, 350.0));
   Source.Append ((100.0, 250.0));
   Source.Append ((100.0, 200.0));
   Clipper.Append ((100.0, 100.0));
   Clipper.Append ((300.0, 100.0));
   Clipper.Append ((300.0, 300.0));
   Clipper.Append ((100.0, 300.0));
   Result := Clip (Source, Clipper);
   Print (Result);
end Main;
