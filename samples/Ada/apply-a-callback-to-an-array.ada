with Ada.Text_Io;
 with Ada.Integer_text_IO;

 procedure Call_Back_Example is
    -- Purpose: Apply a callback to an array
    -- Output: Prints the squares of an integer array to the console

    -- Define the callback procedure
    procedure Display(Location : Positive; Value : Integer) is
    begin
       Ada.Text_Io.Put("array(");
       Ada.Integer_Text_Io.Put(Item => Location, Width => 1);
       Ada.Text_Io.Put(") = ");
       Ada.Integer_Text_Io.Put(Item => Value * Value, Width => 1);
       Ada.Text_Io.New_Line;
    end Display;

    -- Define an access type matching the signature of the callback procedure
    type Call_Back_Access is access procedure(L : Positive; V : Integer);

    -- Define an unconstrained array type
    type Value_Array is array(Positive range <>) of Integer;

    -- Define the procedure performing the callback
    procedure Map(Values : Value_Array; Worker : Call_Back_Access) is
    begin
       for I in Values'range loop
          Worker(I, Values(I));
       end loop;
    end Map;

    -- Define and initialize the actual array
    Sample : Value_Array := (5,4,3,2,1);

 begin
    Map(Sample, Display'access);
 end Call_Back_Example;
