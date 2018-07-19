with Ada.Containers.Indefinite_Vectors, Ada.Text_IO;

procedure Lalefile is

   package Word_Vec is new Ada.Containers.Indefinite_Vectors
     (Index_Type   => Positive,
      Element_Type => String);

   use type Word_Vec.Vector, Ada.Containers.Count_Type;

   type Words_Type is array (Character) of Word_Vec.Vector;

   procedure Read(Words: out Words_Type) is
      F: Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Open(File => F,
                       Name => "pokemon70.txt",
                       Mode => Ada.Text_IO.In_File);
      loop
         declare
            Word: String := Ada.Text_IO.Get_Line(F);
         begin
            exit when Word = "";
            Words(Word(Word'First)).Append(Word);
         end;
      end loop;
   exception
      when Ada.Text_IO.End_Error => null;
   end Read;

   procedure Write (List: Word_Vec.Vector; Prefix: String := "   ") is
      Copy: Word_Vec.Vector := List;
   begin
      loop
         exit when Copy.Is_Empty;
         Ada.Text_IO.Put_Line(Prefix & Copy.First_Element);
         Copy.Delete_First;
      end loop;
   end Write;

   function Run(Start: Character; Words: Words_Type) return Word_Vec.Vector is
      Result: Word_Vec.Vector := Word_Vec.Empty_Vector;
   begin
      for I in Words(Start).First_Index .. Words(Start).Last_Index loop
         declare
            Word: String := Words(Start).Element(I);
            Dupl: Words_Type := Words;
            Alternative : Word_Vec.Vector;
         begin
            Dupl(Start).Delete(I);
            Alternative := Word & Run(Word(Word'Last), Dupl);
            if Alternative.Length > Result.Length then
               Result := Alternative;
            end if;
         end;
      end loop;
      return Result;
   end Run;

   W: Words_Type;
   A_Vector: Word_Vec.Vector;
   Best: Word_Vec.Vector := Word_Vec.Empty_Vector;

begin
   Read(W);
   Ada.Text_IO.Put("Processing ");
   for Ch in Character range 'a' .. 'z' loop
      Ada.Text_IO.Put(Ch & ", ");
      A_Vector := Run(Ch, W);
      if A_Vector.Length > Best.Length then
         Best := A_Vector;
      end if;
   end loop;
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line("Length of longest Path:" &
                          Integer'Image(Integer(Best.Length)));
   Ada.Text_IO.Put_Line("One such path:");
   Write(Best);
end Lalefile;
