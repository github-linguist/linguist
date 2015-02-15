Sedol_char = "0123456789BCDFGHJKLMNPQRSTVWXYZ"

def char2value(c)
  raise "No vowels" unless Sedol_char.include?(c)
  c.to_i(36)
end

Sedolweight = [1,3,1,7,3,9]

def checksum(sedol)
  raise "Invalid length" unless sedol.size == Sedolweight.size
  sum = sedol.split('').zip(Sedolweight).map { |ch, weight|
            char2value(ch) * weight }.inject(:+)
  ((10 - (sum % 10)) % 10).to_s
end

data = %w{
          710889
          B0YBKJ
          406566
          B0YBLH
          228276
          B0YBKL
          557910
          B0YBKR
          585284
          B0YBKT
          B00030
          C0000
          1234567
          00000A
         }

for sedol in data
  print "%-8s " % sedol
  begin
    puts sedol + checksum(sedol)
  rescue => e
    p e
  end
end
