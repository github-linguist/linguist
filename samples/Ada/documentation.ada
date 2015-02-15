with Ada.Text_Io; use Ada.Text_Io;

generic
   SortName : in String;
   type DataType is (<>);
   type SortArrayType is array (Integer range <>) of DataType;
   with procedure Sort (SortArray : in out SortArrayType;
                        Comp, Write, Ex : in out Natural);

package Instrument is
   -- This generic package essentially accomplishes turning the sort
   --  procedures into first-class functions for this limited purpose.
   --  Obviously it doesn't change the way that Ada works with them;
   --  the same thing would have been much more straightforward to
   --  program in a language that had true first-class functions

   package Dur_Io is new Fixed_Io(Duration);

   procedure TimeSort (Arr : in out SortArrayType);

   procedure Put;

   procedure Put (File : in out File_Type);

end Instrument;
