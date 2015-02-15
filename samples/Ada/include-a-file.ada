with Ada.Text_IO, Another_Package; use Ada.Text_IO;
  -- the with-clause tells the compiler to include the Text_IO package from the Ada standard
  -- and Another_Package. Subprograms from these packages may be called as follows:
  --               Ada.Text_IO.Put_Line("some text");
  --               Another_Package.Do_Something("some text");
  -- The use-clause allows the program author to write a subprogram call shortly as
  --               Put_Line("some text");
