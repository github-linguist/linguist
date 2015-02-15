with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;

procedure Associative_Array is

   -- Instantiate the generic package Ada.Containers.Ordered_Maps

   package Associative_Int is new Ada.Containers.Ordered_Maps(Unbounded_String, Integer);
   use Associative_Int;

   Color_Map : Map;
   Color_Cursor : Cursor;
   Success : Boolean;
   Value : Integer;
begin

   -- Add values to the ordered map

   Color_Map.Insert(To_Unbounded_String("Red"), 10, Color_Cursor, Success);
   Color_Map.Insert(To_Unbounded_String("Blue"), 20, Color_Cursor, Success);
   Color_Map.Insert(To_Unbounded_String("Yellow"), 5, Color_Cursor, Success);

   -- retrieve values from the ordered map and print the value and key
   -- to the screen

   Value := Color_Map.Element(To_Unbounded_String("Red"));
   Ada.Text_Io.Put_Line("Red:" & Integer'Image(Value));
   Value := Color_Map.Element(To_Unbounded_String("Blue"));
   Ada.Text_IO.Put_Line("Blue:" & Integer'Image(Value));
   Value := Color_Map.Element(To_Unbounded_String("Yellow"));
   Ada.Text_IO.Put_Line("Yellow:" & Integer'Image(Value));
end Associative_Array;
