with Ada.Numerics.Generic_Complex_Types;
with Ada.Text_IO.Complex_IO;

procedure Complex_Operations is
   -- Ada provides a pre-defined generic package for complex types
   -- That package contains definitions for composition,
   -- negation, addition, subtraction, multiplication, division,
   -- conjugation, exponentiation, and absolute value, as well as
   -- basic comparison operations.
   -- Ada provides a second pre-defined package for sin, cos, tan, cot,
   -- arcsin, arccos, arctan, arccot, and the hyperbolic versions of
   -- those trigonometric functions.

   -- The package Ada.Numerics.Generic_Complex_Types requires definition
   -- with the real type to be used in the complex type definition.

   package Complex_Types is new Ada.Numerics.Generic_Complex_Types (Long_Float);
   use Complex_Types;
   package Complex_IO is new Ada.Text_IO.Complex_IO (Complex_Types);
   use Complex_IO;
   use Ada.Text_IO;

   A : Complex := Compose_From_Cartesian (Re => 1.0, Im => 1.0);
   B : Complex := Compose_From_Polar (Modulus => 1.0, Argument => 3.14159);
   C : Complex;

begin
   -- Addition
   C := A + B;
   Put("A + B = "); Put(C);
   New_Line;
   -- Multiplication
   C := A * B;
   Put("A * B = "); Put(C);
   New_Line;
   -- Inversion
   C := 1.0 / A;
   Put("1.0 / A = "); Put(C);
   New_Line;
   -- Negation
   C := -A;
   Put("-A = "); Put(C);
   New_Line;
   -- Conjugation
   C := Conjugate (C);
end Complex_Operations;
