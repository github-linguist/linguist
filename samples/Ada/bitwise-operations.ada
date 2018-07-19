with Ada.Text_Io; use Ada.Text_Io;
with Interfaces; use Interfaces;

 procedure Bitwise is
    subtype Byte is Unsigned_8;
    package Byte_Io is new Ada.Text_Io.Modular_Io(Byte);

    A : Byte := 255;
    B : Byte := 170;
    X : Byte := 128;
    N : Natural := 1;

 begin
   Put_Line("A and B = "); Byte_Io.Put(Item => A and B, Base => 2);
   Put_Line("A or B  = "); Byte_IO.Put(Item => A or B, Base => 2);
   Put_Line("A xor B = "); Byte_Io.Put(Item => A xor B, Base => 2);
   Put_Line("Not A   = "); Byte_IO.Put(Item => not A, Base => 2);
   New_Line(2);
   Put_Line(Unsigned_8'Image(Shift_Left(X, N))); -- Left shift
   Put_Line(Unsigned_8'Image(Shift_Right(X, N))); -- Right shift
   Put_Line(Unsigned_8'Image(Shift_Right_Arithmetic(X, N))); -- Right Shift Arithmetic
   Put_Line(Unsigned_8'Image(Rotate_Left(X, N))); -- Left rotate
   Put_Line(Unsigned_8'Image(Rotate_Right(X, N))); -- Right rotate
 end bitwise;
