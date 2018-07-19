declare
   X : String := "Hello"; -- Create and initialize a local variable
   Y : Integer; -- Create an uninitialized variable
   Z : Integer renames Y: -- Rename Y (creates a view)
begin
   Y := 1; -- Assign variable
end; -- End of the scope
