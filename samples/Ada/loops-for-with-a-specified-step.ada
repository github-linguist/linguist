with Loopers;
use Loopers;


procedure For_Main is
begin
        Looper_1;
        Looper_2;
        Looper_3;
end For_Main;


package Loopers is
        procedure Looper_1;
        procedure Looper_2;
        procedure Looper_3;
end Loopers;

with Ada.Text_IO, Ada.Integer_Text_IO;
use Ada.Text_IO, Ada.Integer_Text_IO;

package body Loopers is
        procedure Looper_1 is
                Values : array(1..5) of Integer := (2,4,6,8,10);
        begin
                for I in Values'Range loop
                        Put(Values(I),0);
                        if I = Values'Last then
                                Put_Line(".");
                        else
                                Put(",");
                        end if;
                end loop;
        end Looper_1;

        procedure Looper_2 is
                E : Integer := 5;
        begin
                for I in 1..E loop
                        Put(I*2,0);
                        if I = E then
                                Put_Line(".");
                        else
                                Put(",");
                        end if;
                end loop;
        end Looper_2;

        procedure Looper_3 is
                Values : array(1..10) of Integer := (1,2,3,4,5,6,7,8,9,10);
                Indices : array(1..5) of Integer := (2,4,6,8,10);
        begin
                for I in Indices'Range loop
                        Put(Values(Indices(I)),0);
                        if I = Indices'Last then
                                Put_Line(".");
                        else
                                Put(",");
                        end if;
                end loop;
        end Looper_3;

end Loopers;
