-- Example Ada specification file (.ads).
-- Just some random types and declarations.

package Example is

   type Constrained_Index is range 1 .. 10;

   type Record_Type is record
      X, Y        : Natural := 0;
      Z_Index     : Constrained_Index := 1;

   type Records is array (Positive range <>) of Record_Type;

   function Hypot (X, Y : Integer) return Float;

private

   procedure Shift (Dx, Dy : Integer, R : in out Record_Type);

end;

