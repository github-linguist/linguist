# Without restrictions
BEGIN {
   v = [ ]
   m = 0
}

n = $_.length
if n == m then
   v <<= $_
elsif n > m then
   v = [$_]
   m = n
end

END {
   v.each { |s| puts s }
}
