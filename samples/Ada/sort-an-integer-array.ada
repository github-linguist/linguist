with Gnat.Heap_Sort_G;

procedure Integer_Sort is
   -- Heap sort package requires data to be in index values starting at
   -- 1 while index value 0 is used as temporary storage
   type Int_Array is array(Natural range <>) of Integer;
   Values : Int_Array := (0,1,8,2,7,3,6,4,5);

   -- define move and less than subprograms for use by the heap sort package
   procedure Move_Int(From : Natural; To : Natural) is
   begin
      Values(To) := Values(From);
   end Move_Int;

   function Lt_Int(Left, Right : Natural) return Boolean is
   begin
      return Values(Left) < Values (Right);
   end Lt_Int;

   -- Instantiate the generic heap sort package
   package Heap_Sort is new Gnat.Heap_Sort_G(Move_Int, Lt_Int);

begin
   Heap_Sort.Sort(8);
end Integer_Sort;

requires an Ada05 compiler, e.g GNAT GPL 2007
with Ada.Containers.Generic_Array_Sort;

procedure Integer_Sort is
   --
   type Int_Array is array(Natural range <>) of Integer;
   Values : Int_Array := (0,1,8,2,7,3,6,4,5);

   -- Instantiate the generic sort package from the standard Ada library
   procedure Sort is new Ada.Containers.Generic_Array_Sort
     (Index_Type   => Natural,
      Element_Type => Integer,
      Array_Type   => Int_Array);

begin
   Sort(Values);
end Integer_Sort;
