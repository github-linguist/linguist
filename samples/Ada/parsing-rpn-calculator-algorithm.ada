with Ada.Text_IO, Ada.Containers.Vectors;

procedure RPN_Calculator is

  package IIO is new Ada.Text_IO.Float_IO(Float);

   package Float_Vec is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Float);
   Stack: Float_Vec.Vector;

   Input: String := Ada.Text_IO.Get_Line;
   Cursor: Positive := Input'First;
   New_Cursor: Positive;

begin
   loop
      -- read spaces
      while Cursor <= Input'Last and then Input(Cursor)=' ' loop
         Cursor := Cursor + 1;
      end loop;

      exit when Cursor > Input'Last;

      New_Cursor := Cursor;
      while New_Cursor <= Input'Last and then Input(New_Cursor) /= ' ' loop
         New_Cursor := New_Cursor + 1;
      end loop;

      -- try to read a number and push it to the stack
      declare
         Last: Positive;
         Value: Float;
         X, Y: Float;
      begin
         IIO.Get(From => Input(Cursor .. New_Cursor - 1),
                 Item => Value,
                 Last => Last);
         Stack.Append(Value);
         Cursor := New_Cursor;

      exception -- if reading the number fails, try to read an operator token
         when others =>
            Y := Stack.Last_Element; Stack.Delete_Last; -- pick two elements
            X := Stack.Last_Element; Stack.Delete_Last; -- from the stack
            case Input(Cursor) is
               when '+' => Stack.Append(X+Y);
               when '-' => Stack.Append(X-Y);
               when '*' => Stack.Append(X*Y);
               when '/' => Stack.Append(X/Y);
               when '^' => Stack.Append(X ** Integer(Float'Rounding(Y)));
               when others => raise Program_Error with "unecpected token '"
                  & Input(Cursor) & "' at column" & Integer'Image(Cursor);
            end case;
            Cursor := New_Cursor;
      end;

      for I in Stack.First_Index .. Stack.Last_Index loop
         Ada.Text_IO.Put(" ");
         IIO.Put(Stack.Element(I), Aft => 5, Exp => 0);
      end loop;
      Ada.Text_IO.New_Line;
   end loop;

   Ada.Text_IO.Put("Result = ");
   IIO.Put(Item => Stack.Last_Element, Aft => 5, Exp => 0);


end RPN_Calculator;
