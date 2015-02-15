with Ada.Integer_Text_IO, Ada.Text_IO;
 procedure Introspection is
    use Ada.Integer_Text_IO, Ada.Text_IO;
 begin
    Put ("Integer range: ");
    Put (Integer'First);
    Put (" .. ");
    Put (Integer'Last);
    New_Line;

    Put ("Float digits: ");
    Put (Float'Digits);
    New_Line;
 end Introspection;
