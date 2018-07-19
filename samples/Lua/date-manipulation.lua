str = string.lower( "March 7 2009 7:30pm EST" )

month = string.match( str, "%a+" )
if     month == "january"   then month = 1
elseif month == "february"  then month = 2
elseif month == "march"     then month = 3
elseif month == "april"     then month = 4
elseif month == "may"       then month = 5
elseif month == "june"      then month = 6
elseif month == "july"      then month = 7
elseif month == "august"    then month = 8
elseif month == "september" then month = 9
elseif month == "october"   then month = 10
elseif month == "november"  then month = 11
elseif month == "december"  then month = 12
end

strproc = string.gmatch( str, "%d+" )
day  = strproc()
year = strproc()
hour = strproc()
min  = strproc()

if string.find( str, "pm" ) then hour = hour + 12 end

print( os.date( "%c", os.time{ year=year, month=month, day=day, hour=hour, min=min, sec=0 } + 12 * 3600 ) )
