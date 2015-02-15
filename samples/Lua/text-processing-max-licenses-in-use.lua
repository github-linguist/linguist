filename = "mlijobs.txt"
io.input( filename )

max_out, n_out = 0, 0
occurr_dates = {}

while true do
    line = io.read( "*line" )
    if line == nil then break end

    if string.find( line, "OUT" ) ~= nil then
        n_out = n_out + 1
        if n_out > max_out then
            max_out = n_out
            occurr_dates = {}
            occurr_dates[#occurr_dates+1] = string.match( line, "@ ([%d+%p]+)" )
        elseif n_out == max_out then
            occurr_dates[#occurr_dates+1] = string.match( line, "@ ([%d+%p]+)" )
        end
    else
        n_out = n_out - 1
    end
end

print( "Maximum licenses in use:", max_out )
print( "Occurrences:" )
for i = 1, #occurr_dates do
    print( "", occurr_dates[i] )
end
