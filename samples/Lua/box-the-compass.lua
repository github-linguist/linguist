-- List of abbreviated compass point labels
compass_points = { "N", "NbE", "N-NE", "NEbN", "NE", "NEbE", "E-NE", "EbN",
                   "E", "EbS", "E-SE", "SEbE", "SE", "SEbS", "S-SE", "SbE",
                   "S", "SbW", "S-SW", "SWbS", "SW", "SWbW", "W-SW", "WbS",
                   "W", "WbN", "W-NW", "NWbW", "NW", "NWbN", "N-NW", "NbW" }

-- List of angles to test
test_angles = {  0.00,  16.87,  16.88,  33.75,  50.62,  50.63,  67.50,
                84.37,  84.38, 101.25, 118.12, 118.13, 135.00, 151.87,
               151.88, 168.75, 185.62, 185.63, 202.50, 219.37, 219.38,
               236.25, 253.12, 253.13, 270.00, 286.87, 286.88, 303.75,
               320.62, 320.63, 337.50, 354.37, 354.38 }


-- capitalize a string
function capitalize(s)
  return s:sub(1,1):upper() .. s:sub(2)
end

-- convert compass point abbreviation to full text of label
function expand_point(abbr)
  for from, to in pairs( { N="north", E="east", S="south", W="west",
                             b=" by " }) do
    abbr = abbr:gsub(from, to)
  end
  return capitalize(abbr)
end

-- modulus function that returns 1..N instead of 0..N-1
function adjusted_modulo(n, d)
  return 1 + (n - 1) % d
end

-- convert a compass angle from degrees into a box index (1..32)
function compass_point(degrees)
  degrees = degrees % 360
  return adjusted_modulo(1 + math.floor( (degrees+5.625) / 11.25), 32)
end

--  Now output the table of test data
header_format = "%-7s | %-18s | %s"
row_format = "%7.2f | %-18s | %2d"
print(header_format:format("Degrees", "Closest Point", "Index"))
for i, angle in ipairs(test_angles) do
  index = compass_point(angle)
  abbr  = compass_points[index]
  label  = expand_point(abbr)
  print(row_format:format(angle, label, index))
end
