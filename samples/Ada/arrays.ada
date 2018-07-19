procedure Array_Test is

   A, B : array (1..20) of Integer;

   -- Ada array indices may begin at any value, not just 0 or 1
   C : array (-37..20) of integer

   -- Ada arrays may be indexed by enumerated types, which are
   -- discrete non-numeric types
   type Days is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);
   type Activities is (Work, Fish);
   type Daily_Activities is array(Days) of Activities;
   This_Week : Daily_Activities := (Mon..Fri => Work, Others => Fish);

   -- Or any numeric type
   type Fingers is range 1..4; -- exclude thumb
   type Fingers_Extended_Type is array(fingers) of Boolean;
   Fingers_Extended : Fingers_Extended_Type;

   -- Array types may be unconstrained. The variables of the type
   -- must be constrained
   type Arr is array (Integer range <>) of Integer;
   Uninitialized : Arr (1 .. 10);
   Initialized_1 : Arr (1 .. 20) := (others => 1);
   Initialized_2 : Arr := (1 .. 30 => 2);
   Const         : constant Arr := (1 .. 10 => 1, 11 .. 20 => 2, 21 | 22 => 3);
   Centered      : Arr (-50..50) := (0 => 1, Others => 0);

   Result        : Integer
begin

   A := (others => 0);     -- Assign whole array
   B := (1 => 1, 2 => 1, 3 => 2, others => 0);
                           -- Assign whole array, different values
   A (1) := -1;            -- Assign individual element
   A (2..4) := B (1..3);   -- Assign a slice
   A (3..5) := (2, 4, -1); -- Assign a constant slice
   A (3..5) := A (4..6);   -- It is OK to overlap slices when assigned

   Fingers_Extended'First := False; -- Set first element of array
   Fingers_Extended'Last := False;  -- Set last element of array

end Array_Test;
