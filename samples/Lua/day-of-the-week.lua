require("date")

for year=2008,2121 do
   if date(year, 12, 25):getweekday() == 1 then
      print(year)
   end
end
