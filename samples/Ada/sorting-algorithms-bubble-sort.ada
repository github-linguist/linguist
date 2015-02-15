generic
 type Element is private;
 with function "=" (E1, E2 : Element) return Boolean is <>;
 with function "<" (E1, E2 : Element) return Boolean is <>;
 type Index is (<>);
 type Arr is array (Index range <>) of Element;
procedure Bubble_Sort (A : in out Arr);

procedure Bubble_Sort (A : in out Arr) is
 Finished : Boolean;
 Temp     : Element;
begin
 loop
  Finished := True;
  for J in A'First .. Index'Pred (A'Last) loop
   if A (Index'Succ (J)) < A (J) then
    Finished := False;
    Temp := A (Index'Succ (J));
    A (Index'Succ (J)) := A (J);
    A (J) := Temp;
   end if;
  end loop;
  exit when Finished;
 end loop;
end Bubble_Sort;

--  Example of usage:
with Ada.Text_IO; use Ada.Text_IO;
with Bubble_Sort;
procedure Main is
 type Arr is array (Positive range <>) of Integer;
 procedure Sort is new
  Bubble_Sort
   (Element => Integer,
    Index   => Positive,
    Arr     => Arr);
 A : Arr := (1, 3, 256, 0, 3, 4, -1);
begin
 Sort (A);
 for J in A'Range loop
  Put (Integer'Image (A (J)));
 end loop;
 New_Line;
end Main;
