with Ada.Strings.Hash;
with Ada.Containers.Hashed_Maps;
with Ada.Text_Io;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Hash_Map_Test is
   function Equivalent_Key (Left, Right : Unbounded_String) return Boolean is
   begin
      return Left = Right;
   end Equivalent_Key;

   function Hash_Func(Key : Unbounded_String) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Hash(To_String(Key));
   end Hash_Func;

   package My_Hash is new Ada.Containers.Hashed_Maps(Key_Type => Unbounded_String,
      Element_Type => Unbounded_String,
      Hash => Hash_Func,
      Equivalent_Keys => Equivalent_Key);

   type String_Array is array(Positive range <>) of Unbounded_String;

   Hash : My_Hash.Map;
   Key_List : String_Array := (To_Unbounded_String("foo"),
      To_Unbounded_String("bar"),
      To_Unbounded_String("val"));

   Element_List : String_Array := (To_Unbounded_String("little"),
      To_Unbounded_String("miss"),
      To_Unbounded_String("muffet"));

begin
   for I in Key_List'range loop
      Hash.Insert(Key => (Key_List(I)),
         New_Item => (Element_List(I)));
   end loop;
   for I in Key_List'range loop
      Ada.Text_Io.Put_Line(To_String(Key_List(I)) & " => " &
         To_String(Hash.Element(Key_List(I))));
   end loop;

end Hash_Map_Test;
