with Ada.Containers.Indefinite_Vectors, Ada.Text_IO;

procedure Here_Doc is

   package String_Vec is new Ada.Containers.Indefinite_Vectors
     (Index_Type   => Positive,
      Element_Type => String);

   use type String_Vec.Vector;

   Document: String_Vec.Vector := String_Vec.Empty_Vector
    & "This is a vector of strings with the following properties:"
    & "  - indention is preserved, and"
    & "  - a quotation mark '""' must be ""escaped"" by a double-quote '""""'.";
begin
   Document := Document & "Have fun!";
   for I in Document.First_Index .. Document.Last_Index loop
      Ada.Text_IO.Put_Line(Document.Element(I));
   end loop;
end Here_Doc;
