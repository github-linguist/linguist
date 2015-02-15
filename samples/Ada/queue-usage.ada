with FIFO;
with Ada.Text_Io; use Ada.Text_Io;

procedure Queue_Test is
   package Int_FIFO is new FIFO (Integer);
   use Int_FIFO;
   Queue : FIFO_Type;
   Value : Integer;
begin
   Push (Queue, 1);
   Push (Queue, 2);
   Push (Queue, 3);
   Pop (Queue, Value);
   Pop (Queue, Value);
   Push (Queue, 4);
   Pop (Queue, Value);
   Pop (Queue, Value);
   Push (Queue, 5);
   Pop (Queue, Value);
   Put_Line ("Is_Empty " & Boolean'Image (Is_Empty (Queue)));
end Queue_Test;
