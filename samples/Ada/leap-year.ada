-- Incomplete code, just a sniplet to do the task. Can be used in any package or method.
-- Adjust the type of Year if you use a different one.
function Is_Leap_Year (Year : Integer) return Boolean is
begin
   if Year rem 100 = 0 then
      return Year rem 400 = 0;
   else
      return Year rem 4 = 0;
   end if;
end Is_Leap_Year;


-- Should be faster without conditional and only one remainder (the other two are really binary and)
function Is_Leap_Year (Year : Integer) return Boolean is
begin
   return (Year rem 4 = 0) and ((Year rem 100 /= 0) or (Year rem 16 = 0));
end Is_Leap_Year;


-- To improve speed a bit more, use with
pragma Inline (Is_Leap_Year);
